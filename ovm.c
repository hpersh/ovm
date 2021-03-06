#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>

#include "ovm.h"

static void __ovm_inst_free (ovm_t ovm, ovm_inst_t inst);
static ovm_inst_t __ovm_inst_retain (ovm_t ovm, ovm_inst_t inst);
static void __ovm_inst_release (ovm_t ovm, ovm_inst_t inst);

static void _ovm_inst_alloc (ovm_t ovm, ovm_class_t cl, ovm_inst_t * dst);


unsigned
ovm_is_subclass_of (ovm_class_t cl1, ovm_class_t cl2)
{
  for (; cl1; cl1 = cl1->parent)
    {
      if (cl1 == cl2)
	return (1);
    }

  return (0);
}

unsigned
ovm_is_kind_of (ovm_inst_t inst, ovm_class_t cl)
{
  return (ovm_is_subclass_of (ovm_inst_of (inst), cl));
}

void
_ovm_frame_enter (ovm_t ovm, struct ovm_frame *fr, unsigned size)
{
  unsigned data_size = size - sizeof (struct ovm_frame);

  fr->prev = ovm->frp;
  fr->size = data_size / sizeof (ovm_inst_t);
#ifndef NDEBUG
  fr->start = (ovm_inst_t *) (fr + 1);
  fr->end = fr->start + data_size;
#endif
  memset (fr + 1, 0, data_size);
  ovm->frp = fr;
}

void
_ovm_frame_leave (ovm_t ovm)
{
  struct ovm_frame *fr = ovm->frp;
  ovm_inst_t *p;
  unsigned n;

  for (p = (ovm_inst_t *) (fr + 1), n = fr->size; n; --n, ++p)
    __ovm_inst_release (ovm, *p);

  ovm->frp = fr->prev;
}





void
ovm_cval_get (ovm_t ovm, ovm_cval_t dst, ovm_inst_t inst)
{
  ovm_class_t cl = ovm_inst_of (inst);

  if (cl == ovm_cl_bool)
    {
      dst->boolval = BOOLVAL (inst);
      return;
    }
  if (cl == ovm_cl_integer)
    {
      dst->intval = INTVAL (inst);
      return;
    }
  if (cl == ovm_cl_float)
    {
      dst->floatval = FLOATVAL (inst);
      return;
    }
  if (ovm_is_subclass_of (cl, ovm_cl_string))
    {
      dst->strval->size = STRVAL (inst)->size;
      *(char **) &dst->strval->data = STRVAL (inst)->data;

      return;
    }

  if (cl == ovm_cl_bmap)
    {
      dst->bmval->size = BMVAL (inst)->size;
      *(ovm_bmval_unit_t **) & dst->bmval->data = BMVAL (inst)->data;

      return;
    }

  OVM_ASSERT (0);
}

#define ADD_MAX(_sum, _max, _val)  do { if (((_sum) += (_val)) > (_max))  (_max) = (_sum); } while (0)

static void
_ovm_malloc (ovm_t ovm, unsigned size, void **result)
{
  void *p = malloc (size);

  OVM_ASSERT (p != 0);

  ADD_MAX (ovm->stats->mem_in_use, ovm->stats->mem_max, size);

  *result = p;
}

static void
_ovm_zmalloc (ovm_t ovm, unsigned size, void **result)
{
  _ovm_malloc (ovm, size, result);

  memset (*result, 0, size);
}

static void
_ovm_free (ovm_t ovm, void **p, unsigned size)
{
  void *q = *p;

  if (q)
    {
      free (q);

      ovm->stats->mem_in_use -= size;

      *p = 0;
    }
}

enum
{
  _OVM_INST_MAGIC = 0x48504f564d494e53ULL
};

static void
_ovm_inst_alloc (ovm_t ovm, ovm_class_t cl, ovm_inst_t * dst)
{
  ovm_inst_t r;

  if (list_empty (ovm->insts_free))
    {
      struct ovm_inst_page *p;
      ovm_inst_t q;
      unsigned n;

      _ovm_malloc (ovm, ovm->inst_page_size, (void **) &p);

      ADD_MAX (ovm->stats->pages_in_use, ovm->stats->pages_max, 1);

      p->in_use_cnt = 0;

      list_insert (p->list_node, list_end (ovm->inst_pages));

      for (q = (ovm_inst_t) (p + 1), n = ovm->insts_per_page; n; --n, ++q)
	{
	  q->inst_page = p;
#ifndef NDEBUG
	  q->magic = 0;
#endif

	  list_insert (q->list_node, list_end (ovm->insts_free));
	}
    }

  r =
    FIELD_PTR_TO_STRUCT_PTR (list_first (ovm->insts_free), struct ovm_inst,
			     list_node);

  list_erase (r->list_node);
  list_insert (r->list_node, list_end (ovm->insts_in_use));

#ifndef NDEBUG
  r->magic = _OVM_INST_MAGIC;
#endif
  r->ref_cnt = 0;
  r->inst_of = cl;
  memset (r->val, 0, sizeof (r->val));

  ++r->inst_page->in_use_cnt;

  ADD_MAX (ovm->stats->insts_in_use, ovm->stats->insts_max, 1);

  _ovm_assign (ovm, dst, r);
}


static void
__ovm_inst_free (ovm_t ovm, ovm_inst_t inst)
{
  struct ovm_inst_page *p = inst->inst_page;
  ovm_inst_t q;
  unsigned n;

  list_erase (inst->list_node);
  list_insert (inst->list_node, list_end (ovm->insts_free));

#ifndef NDEBUG
  inst->magic = 0;
#endif

  --ovm->stats->insts_in_use;

  if (--p->in_use_cnt == 0)
    {
      for (q = (ovm_inst_t) (p + 1), n = ovm->insts_per_page; n; --n, ++q)
	{
	  list_erase (q->list_node);
	}

      list_erase (p->list_node);

      _ovm_free (ovm, (void **) &p, ovm->inst_page_size);

      --ovm->stats->pages_in_use;
    }
}

static ovm_inst_t
__ovm_inst_retain (ovm_t ovm, ovm_inst_t inst)
{
  if (inst)
    {
      ++inst->ref_cnt;

      OVM_DEBUG_ASSERT (inst->ref_cnt != 0);
    }

  return (inst);
}

static void
__ovm_inst_release (ovm_t ovm, ovm_inst_t inst)
{
  if (inst)
    {
      OVM_DEBUG_ASSERT (inst->ref_cnt != 0);

      if (--inst->ref_cnt == 0)
	{
	  ovm_class_t cl = ovm_inst_of (inst);

	  (*cl->walk) (ovm, cl, inst, __ovm_inst_release);
	  (*cl->free) (ovm, cl, inst);
	}
    }
}

void
_ovm_assign (ovm_t ovm, ovm_inst_t * dst, ovm_inst_t src)
{
  ovm_inst_t tmp;

  tmp = *dst;
  *dst = __ovm_inst_retain (ovm, src);
  __ovm_inst_release (ovm, tmp);
}

static void inline
_ovm_init_parent (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst,
		  unsigned argc, ovm_inst_t * argv)
{
  cl = cl->parent;

  (*cl->init) (ovm, cl, inst, argc, argv);
}

static inline void
_ovm_walk_parent (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst,
		  void (*func) (struct ovm *, ovm_inst_t))
{
  cl = cl->parent;

  (*cl->walk) (ovm, cl, inst, func);
}

static inline void
_ovm_free_parent (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst)
{
  cl = cl->parent;

  (*cl->free) (ovm, cl, inst);
}


struct ovm *
ovm_init (ovm_t ovm, unsigned inst_page_size)
{
  memset (ovm, 0, sizeof (*ovm));

  ovm->inst_page_size = inst_page_size;
  ovm->insts_per_page =
    (inst_page_size -
     sizeof (struct ovm_inst_page)) / sizeof (struct ovm_inst);

  list_init (ovm->inst_pages);

  list_init (ovm->insts_free);
  list_init (ovm->insts_in_use);

  ovm->frp = 0;

  return (ovm);
}

#ifndef NDEBUG
static inline void
_ovm_dst_chk (ovm_t ovm, ovm_inst_t * dst)
{
  struct ovm_frame *p;

  for (p = ovm->frp; p; p = p->prev)
    {
      if (dst >= p->start && dst < p->end)
	return;
    }

  OVM_ASSERT (0);
}
#else
#define _ovm_dst_chk(ovm, dst)
#endif

#ifndef NDEBUG
static inline ovm_inst_t
_ovm_inst_chk (ovm_inst_t inst)
{
  OVM_ASSERT (inst == 0 || inst->magic == _OVM_INST_MAGIC);

  return (inst);
}
#else
#define _ovm_inst_chk(inst)  (inst)
#endif

void
_ovm_inst_new (ovm_t ovm, ovm_inst_t * dst, ovm_class_t cl, unsigned argc,
	       ...)
{
  va_list ap;
  ovm_inst_t argv[argc], *p;
  unsigned n;

  va_start (ap, argc);

  _ovm_dst_chk (ovm, dst);

  for (p = argv, n = argc; n; --n, ++p)
    {
      *p = _ovm_inst_chk (va_arg (ap, ovm_inst_t));
    }

  (*cl->new) (ovm, cl, dst, argc, argv);

  va_end (ap);
}

static void
_ovm_method_call_run (ovm_t ovm, ovm_inst_t * dst, ovm_inst_t rcvr,
		      ovm_class_t cl, unsigned sel, unsigned argc, va_list ap)
{
  unsigned n;
  ovm_inst_t argv[1 + argc], *p;
  void (*f) (struct ovm *, ovm_inst_t *, unsigned, ovm_inst_t *);

  _ovm_dst_chk (ovm, dst);

  argv[0] = _ovm_inst_chk (rcvr);

  for (p = &argv[1], n = argc; n; --n, ++p)
    {
      *p = _ovm_inst_chk (va_arg (ap, ovm_inst_t));
    }

  OVM_ASSERT (sel < OVM_INST_METHOD_NUM_SELS);

  for (f = 0; cl; cl = cl->parent)
    {
      if (f = cl->inst_method_func_tbl[sel])
	break;
    }

  OVM_ASSERT (f != 0);

  (*f) (ovm, dst, argc, argv);
}

void
_ovm_inst_method_call_cl (ovm_t ovm, ovm_inst_t * dst, ovm_inst_t rcvr,
			  ovm_class_t cl, unsigned sel, unsigned argc, ...)
{
  va_list ap;

  va_start (ap, argc);

  _ovm_method_call_run (ovm, dst, rcvr, cl, sel, argc, ap);

  va_end (ap);
}

void
_ovm_inst_method_call (ovm_t ovm, ovm_inst_t * dst, ovm_inst_t rcvr,
		       unsigned sel, unsigned argc, ...)
{
  va_list ap;

  va_start (ap, argc);

  _ovm_method_call_run (ovm, dst, rcvr, ovm_inst_of (rcvr), sel, argc, ap);

  va_end (ap);
}

static void
_ovm_inst_new2 (ovm_t ovm, ovm_class_t cl, ovm_inst_t * dst, unsigned argc,
		ovm_inst_t * argv)
{
  OVM_FRAME_DECL (fr, work);

  OVM_FRAME_ENTER (ovm, fr);

  _ovm_inst_alloc (ovm, cl, &fr->work);

  (*cl->init) (ovm, cl, fr->work, argc, argv);

  _ovm_assign (ovm, dst, fr->work);

  OVM_FRAME_LEAVE (ovm);
}

static void
_ovm_inst_new1 (ovm_t ovm, ovm_class_t cl, ovm_inst_t * dst, unsigned argc,
		ovm_inst_t * argv)
{
  if (argc == 1 && ovm_inst_of (argv[0]) == cl)
    {
      _ovm_assign (ovm, dst, argv[0]);
    }
  else
    {
      _ovm_inst_new2 (ovm, cl, dst, argc, argv);
    }
}

static void
_slice (ovm_intval_t * ofsp, ovm_intval_t * lenp, ovm_intval_t size)
{
  ovm_intval_t ofs = *ofsp, len = *lenp;

  if (ofs < 0)
    ofs = size + ofs;

  if (len < 0)
    {
      ofs += len;
      len = -len;
    }

  OVM_ASSERT (ofs >= 0 && (ofs + len) <= size);

  *ofsp = ofs;
  *lenp = len;
}

static void
_whitespace_skip (char **s, unsigned *n)
{
  char *p = *s;
  unsigned k = *n;

  for (; k; --k, ++p)
    {
      if (!isspace (*p))
	break;
    }

  *s = p;
  *n = k;
}

static unsigned __ovm_bmap_init (ovm_t ovm, ovm_inst_t inst, unsigned size);
static unsigned __ovm_list_len (ovm_inst_t inst);

static inline unsigned
bit (unsigned n)
{
  return (1 << n);
}

static inline unsigned
bits (unsigned n)
{
  return (n == 32 ? (unsigned) -1 : bit (n) - 1);
}

static unsigned
bmap_unit_idx (unsigned bit)
{
  return (bit >> OVM_BMVAL_UNIT_BITS_LOG2);
}

static unsigned
bmap_unit_sh (unsigned bit)
{
  return (bit & (OVM_BMVAL_UNIT_BITS - 1));
}

static unsigned
bmap_bits_to_units (unsigned bits)
{
  OVM_ASSERT (bits > 0);

  return (bmap_unit_idx (bits - 1) + 1);
}

static unsigned
bmap_units_to_bytes (unsigned units)
{
  return (units * sizeof (ovm_bmval_unit_t));
}


/***************************************************************************/

static void
_ovm_object_init (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst,
		  unsigned argc, ovm_inst_t * argv)
{
  OVM_ASSERT (argc == 0);
}

static void
_ovm_object_walk (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst,
		  void (*func) (struct ovm *, ovm_inst_t))
{
}

static void
_ovm_object_free (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst)
{
  __ovm_inst_free (ovm, inst);
}

struct ovm_class ovm_cl_object[1] = { {
				       .name = "Object",
				       .parent = 0,
				       .init = _ovm_object_init,
				       .walk = _ovm_object_walk,
				       .free = _ovm_object_free,
				       }
};

/***************************************************************************/

static void
__ovm_bool_newc (ovm_t ovm, ovm_inst_t * dst, ovm_boolval_t val)
{
  _ovm_inst_alloc (ovm, ovm_cl_bool, dst);

  BOOLVAL (*dst) = (val != 0);
}

void
_ovm_bool_newc (ovm_t ovm, ovm_inst_t * dst, ovm_boolval_t val)
{
  _ovm_dst_chk (ovm, dst);

  __ovm_bool_newc (ovm, dst, val);
}

static void
_xml_parse_bool2 (ovm_t ovm, ovm_inst_t inst, char *s, unsigned n)
{
  char c;

  _whitespace_skip (&s, &n);
  OVM_ASSERT (n >= 1);
  c = *s;
  OVM_ASSERT (c == '0' || c == '1');

  BOOLVAL (inst) = (c == '1');

  ++s;
  --n;
  _whitespace_skip (&s, &n);
  OVM_ASSERT (n >= 10 && strncmp (s, "</Boolean>", 10) == 0);
  s += 10;
  n -= 10;
  _whitespace_skip (&s, &n);
  OVM_ASSERT (n == 0);
}

static void
_xml_parse_bool (ovm_t ovm, ovm_inst_t inst, char *s, unsigned n)
{
  _whitespace_skip (&s, &n);
  OVM_ASSERT (n >= 9 && strncmp (s, "<Boolean>", 9) == 0);
  _xml_parse_bool2 (ovm, inst, s + 9, n - 9);
}

static void
_ovm_bool_init (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst, unsigned argc,
		ovm_inst_t * argv)
{
  if (argc > 0)
    {
      ovm_inst_t arg = argv[0];
      ovm_class_t arg_cl = ovm_inst_of (arg);

      if (arg_cl == ovm_cl_integer)
	{
	  BOOLVAL (inst) = (INTVAL (arg) != 0);
	}
      else if (arg_cl == ovm_cl_string)
	{
	  BOOLVAL (inst) = (STRVAL (arg)->size > 1);
	}
      else if (arg_cl == ovm_cl_xml)
	{
	  _xml_parse_bool (ovm, inst, STRVAL (arg)->data,
			   STRVAL (arg)->size - 1);
	}
      else
	{
	  OVM_ASSERT (0);
	}

      --argc;
      ++argv;
    }

  _ovm_init_parent (ovm, cl, inst, argc, argv);
}

static void
_ovm_bool_not (ovm_t ovm, ovm_inst_t * dst, unsigned argc, ovm_inst_t * argv)
{
  OVM_ASSERT (argc == 0);
  OVM_ASSERT (ovm_is_kind_of (argv[0], ovm_cl_bool));

  __ovm_bool_newc (ovm, dst, !BOOLVAL (argv[0]));
}

static void
_ovm_bool_and (ovm_t ovm, ovm_inst_t * dst, unsigned argc, ovm_inst_t * argv)
{
  OVM_ASSERT (argc == 1);
  OVM_ASSERT (ovm_is_kind_of (argv[0], ovm_cl_bool));
  OVM_ASSERT (ovm_is_kind_of (argv[1], ovm_cl_bool));

  __ovm_bool_newc (ovm, dst, BOOLVAL (argv[0]) && BOOLVAL (argv[1]));
}

static void
_ovm_bool_or (ovm_t ovm, ovm_inst_t * dst, unsigned argc, ovm_inst_t * argv)
{
  OVM_ASSERT (argc == 1);
  OVM_ASSERT (ovm_is_kind_of (argv[0], ovm_cl_bool));
  OVM_ASSERT (ovm_is_kind_of (argv[1], ovm_cl_bool));

  __ovm_bool_newc (ovm, dst, BOOLVAL (argv[0]) || BOOLVAL (argv[1]));
}

static void
_ovm_bool_xor (ovm_t ovm, ovm_inst_t * dst, unsigned argc, ovm_inst_t * argv)
{
  OVM_ASSERT (argc == 1);
  OVM_ASSERT (ovm_is_kind_of (argv[0], ovm_cl_bool));
  OVM_ASSERT (ovm_is_kind_of (argv[1], ovm_cl_bool));

  __ovm_bool_newc (ovm, dst, BOOLVAL (argv[0]) ^ BOOLVAL (argv[1]));
}

struct ovm_class ovm_cl_bool[1] = { {
				     .name = "Boolean",
				     .parent = ovm_cl_object,
				     .new = _ovm_inst_new1,
				     .init = _ovm_bool_init,
				     .walk = _ovm_walk_parent,
				     .free = _ovm_free_parent,
				     .inst_method_func_tbl = {
							      [OVM_INST_METHOD_SEL_AND] = _ovm_bool_and,
							      [OVM_INST_METHOD_SEL_NOT] = _ovm_bool_not,
							      [OVM_INST_METHOD_SEL_OR] = _ovm_bool_or,
							      [OVM_INST_METHOD_SEL_XOR] = _ovm_bool_xor}
				     }
};

/***************************************************************************/

struct ovm_class ovm_cl_num[1] = { {
				    .name = "Number",
				    .parent = ovm_cl_object,
				    /* .new - Not instantiable */
				    .init = _ovm_init_parent,
				    .walk = _ovm_walk_parent,
				    .free = _ovm_free_parent,
				    .inst_method_func_tbl = {
							     }
				    }
};

/***************************************************************************/

static void
__ovm_integer_newc (ovm_t ovm, ovm_inst_t * dst, ovm_intval_t val)
{
  _ovm_inst_alloc (ovm, ovm_cl_integer, dst);

  INTVAL (*dst) = val;
}

void
_ovm_integer_newc (ovm_t ovm, ovm_inst_t * dst, ovm_intval_t val)
{
  _ovm_dst_chk (ovm, dst);

  __ovm_integer_newc (ovm, dst, val);
}

static void
_xml_parse_int2 (ovm_t ovm, ovm_inst_t inst, char *s, unsigned n)
{
  ovm_intval_t val = 0;
  unsigned negf = 0;

  _whitespace_skip (&s, &n);
  if (n > 0 && *s == '-')
    {
      negf = 1;
      ++s;
      --n;
    }

  for (;;)
    {
      char c;

      OVM_ASSERT (n > 0);
      c = *s;
      if (c == '<')
	break;
      if (isspace (c))
	{
	  _whitespace_skip (&s, &n);
	  break;
	}

      OVM_ASSERT (c >= '0' && c <= '9');
      val = 10 * val + (c - '0');

      ++s;
      --n;
    }

  OVM_ASSERT (n >= 10 && strncmp (s, "</Integer>", 10) == 0);
  s += 10;
  n -= 10;
  _whitespace_skip (&s, &n);
  OVM_ASSERT (n == 0);

  if (negf)
    val = -val;
  INTVAL (inst) = val;
}

static void
_xml_parse_int (ovm_t ovm, ovm_inst_t inst, char *s, unsigned n)
{
  _whitespace_skip (&s, &n);
  OVM_ASSERT (n >= 9 && strncmp (s, "<Integer>", 9) == 0);
  _xml_parse_int2 (ovm, inst, s + 9, n - 9);
}

static void
_ovm_integer_init (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst, unsigned argc,
		   ovm_inst_t * argv)
{
  if (argc > 0)
    {
      ovm_intval_t val = 0;
      ovm_inst_t arg = argv[0];
      ovm_class_t arg_cl = ovm_inst_of (arg);

      if (arg_cl == ovm_cl_bool)
	{
	  INTVAL (inst) = (BOOLVAL (arg) != 0);
	}
      else if (arg_cl == ovm_cl_float)
	{
	  INTVAL (inst) = (ovm_intval_t) FLOATVAL (arg);
	}
      else if (arg_cl == ovm_cl_string)
	{
	  OVM_ASSERT (sscanf (STRVAL (arg)->data, "%lld", &INTVAL (inst)) ==
		      1);
	}
      else if (arg_cl == ovm_cl_xml)
	{
	  _xml_parse_int (ovm, inst, STRVAL (arg)->data,
			  STRVAL (arg)->size - 1);
	}
      else
	{
	  OVM_ASSERT (0);
	}

      --argc;
      ++argv;
    }

  _ovm_init_parent (ovm, cl, inst, argc, argv);
}

static void
_ovm_integer_add (ovm_t ovm, ovm_inst_t * dst, unsigned argc,
		  ovm_inst_t * argv)
{
  ovm_intval_t arg;

  OVM_ASSERT (argc == 1);
  OVM_ASSERT (ovm_is_kind_of (argv[0], ovm_cl_integer));
  if (ovm_inst_of (argv[1]) == ovm_cl_integer)
    {
      arg = INTVAL (argv[1]);
    }
  else if (ovm_inst_of (argv[1]) == ovm_cl_float)
    {
      arg = (ovm_intval_t) FLOATVAL (argv[1]);
    }
  else
    {
      OVM_ASSERT (0);
    }

  __ovm_integer_newc (ovm, dst, INTVAL (argv[0]) + arg);
}

static void
_ovm_integer_sub (ovm_t ovm, ovm_inst_t * dst, unsigned argc,
		  ovm_inst_t * argv)
{
  ovm_intval_t arg;

  OVM_ASSERT (argc == 1);
  OVM_ASSERT (ovm_is_kind_of (argv[0], ovm_cl_integer));
  if (ovm_inst_of (argv[1]) == ovm_cl_integer)
    {
      arg = INTVAL (argv[1]);
    }
  else if (ovm_inst_of (argv[1]) == ovm_cl_float)
    {
      arg = (ovm_intval_t) FLOATVAL (argv[1]);
    }
  else
    {
      OVM_ASSERT (0);
    }

  __ovm_integer_newc (ovm, dst, INTVAL (argv[0]) - arg);
}

struct ovm_class ovm_cl_integer[1] = { {
					.name = "Integer",
					.parent = ovm_cl_num,
					.new = _ovm_inst_new1,
					.init = _ovm_integer_init,
					.walk = _ovm_walk_parent,
					.free = _ovm_free_parent,
					.inst_method_func_tbl = {
								 [OVM_INST_METHOD_SEL_ADD] = _ovm_integer_add}
					}
};

/***************************************************************************/

static void
__ovm_float_newc (ovm_t ovm, ovm_inst_t * dst, ovm_floatval_t val)
{
  _ovm_inst_alloc (ovm, ovm_cl_float, dst);

  FLOATVAL (*dst) = val;
}

void
_ovm_float_newc (ovm_t ovm, ovm_inst_t * dst, ovm_floatval_t val)
{
  _ovm_dst_chk (ovm, dst);

  __ovm_float_newc (ovm, dst, val);
}

static void
_xml_parse_float2 (ovm_t ovm, ovm_inst_t inst, char *s, unsigned n)
{
  unsigned mnegf = 0, dcnt = 0, dpf = 0, ef = 0, enegf = 0;
  char *q;
  unsigned k;

  _whitespace_skip (&s, &n);

  for (q = s;;)
    {
      char c;

      OVM_ASSERT (n > 0);
      c = *s;

      if (c == '<')
	{
	  k = s - q;
	  break;
	}
      if (isspace (c))
	{
	  k = s - q;
	  _whitespace_skip (&s, &n);
	  break;
	}

      if (c == '-')
	{
	  OVM_ASSERT (dcnt == 0);
	  if (ef)
	    {
	      OVM_ASSERT (!enegf);
	      enegf = 1;
	    }
	  else
	    {
	      OVM_ASSERT (!mnegf);
	      mnegf = 1;
	    }
	}
      else if (c >= '0' && c <= '9')
	{
	  ++dcnt;
	}
      else if (c == '.')
	{
	  OVM_ASSERT (!ef && !dpf && dcnt > 0);
	  dpf = 1;
	}
      else if ((c | 0x20) == 'e')
	{
	  OVM_ASSERT (!ef && dcnt > 0);
	  ef = 1;
	  dcnt = 0;
	}

      ++s;
      --n;
    }

  OVM_ASSERT (dcnt > 0);
  OVM_ASSERT (n >= 8 && strncmp (s, "</Float>", 8) == 0);
  s += 8;
  n -= 8;
  _whitespace_skip (&s, &n);
  OVM_ASSERT (n == 0);

  {
    char buf[k + 1];

    memcpy (buf, q, k);
    buf[k] = 0;

    OVM_ASSERT (sscanf (buf, "%Lg", &FLOATVAL (inst)) == 1);
  }
}

static void
_xml_parse_float (ovm_t ovm, ovm_inst_t inst, char *s, unsigned n)
{
  _whitespace_skip(&s, &n);
  OVM_ASSERT (n >= 7 && strncmp (s, "<Float>", 7) == 0);
  _xml_parse_float2 (ovm, inst, s + 7, n - 7);
}

static void
_ovm_float_init (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst, unsigned argc,
		 ovm_inst_t * argv)
{
  if (argc > 1)
    {
      ovm_inst_t arg = argv[0];
      ovm_class_t arg_cl = ovm_inst_of (arg);
      ovm_floatval_t val = 0.0;

      if (arg_cl == ovm_cl_integer)
	{
	  FLOATVAL (inst) = (ovm_floatval_t) INTVAL (arg);
	}
      else if (arg_cl == ovm_cl_string)
	{
	  OVM_ASSERT (sscanf (STRVAL (arg)->data, "%Lg", &FLOATVAL (inst)) ==
		      1);
	}
      else if (arg_cl == ovm_cl_xml)
	{
	  _xml_parse_float (ovm, inst, STRVAL (arg)->data,
			    STRVAL (arg)->size - 1);
	}
      else
	{
	  OVM_ASSERT (0);
	}

      --argc;
      ++argv;
    }

  _ovm_init_parent (ovm, cl, inst, argc, argv);
}

static void
_ovm_float_add (ovm_t ovm, ovm_inst_t * dst, unsigned argc, ovm_inst_t * argv)
{
  OVM_ASSERT (argc == 1);
  OVM_ASSERT (ovm_is_kind_of (argv[0], ovm_cl_float));
  OVM_ASSERT (ovm_is_kind_of (argv[1], ovm_cl_float));

  __ovm_float_newc (ovm, dst, FLOATVAL (argv[0]) + FLOATVAL (argv[1]));
}

struct ovm_class ovm_cl_float[1] = { {
				      .name = "Float",
				      .parent = ovm_cl_num,
				      .new = _ovm_inst_new1,
				      .init = _ovm_float_init,
				      .walk = _ovm_walk_parent,
				      .free = _ovm_free_parent,
				      .inst_method_func_tbl = {
							       [OVM_INST_METHOD_SEL_ADD] = _ovm_float_add}
				      }
};

/***************************************************************************/

static struct ovm_strval *
__ovm_strval_initv (ovm_t ovm, struct ovm_strval *dst, unsigned argc,
		    struct ovm_strval *argv)
{
  struct ovm_strval *p;
  unsigned size, n, k;
  char *q;

  for (size = 0, p = argv, n = argc; n; --n, ++p)
    {
      size += p->size - 1;
    }
  ++size;

  dst->size = size;
  _ovm_malloc (ovm, size, (void **) &dst->data);

  for (q = dst->data, p = argv, n = argc; n; --n, ++p)
    {
      k = p->size - 1;
      memcpy (q, p->data, k);
      q += k;
    }
  *q = 0;

  return (dst);
}

static struct ovm_strval *
__ovm_strval_inita (ovm_t ovm, struct ovm_strval *dst, unsigned argc,
		    ovm_inst_t * argv)
{
  struct ovm_strval sv_argv[argc], *p;
  unsigned n;

  for (p = sv_argv, n = argc; n; --n, ++p, ++argv)
    *p = *STRVAL (*argv);

  __ovm_strval_initv (ovm, dst, argc, sv_argv);

  return (dst);
}

static struct ovm_strval *
__ovm_strval_initc (ovm_t ovm, struct ovm_strval *dst, unsigned argc, ...)
{
  va_list ap;
  struct ovm_strval argv[argc], *p;
  unsigned n;

  va_start (ap, argc);

  for (p = argv, n = argc; n; --n, ++p)
    {
      p->size = va_arg (ap, unsigned);
      p->data = va_arg (ap, char *);
    }

  __ovm_strval_initv (ovm, dst, argc, argv);

  va_end (ap);

  return (dst);
}

static void
__ovm_strval_new (ovm_t ovm, ovm_inst_t * dst, struct ovm_strval *sv)
{
  _ovm_inst_alloc (ovm, ovm_cl_string, dst);
  *STRVAL (*dst) = *sv;
}

static void
__ovm_string_newc (ovm_t ovm, ovm_inst_t * dst, char *s)
{
  struct ovm_strval sv[1];

  __ovm_strval_new (ovm, dst,
		    __ovm_strval_initc (ovm, sv, 1, strlen (s) + 1, s));
}

void
_ovm_string_newc (ovm_t ovm, ovm_inst_t * dst, char *s)
{
  _ovm_dst_chk (ovm, dst);

  __ovm_string_newc (ovm, dst, s);
}

static void
_xml_parse_string2 (ovm_t ovm, ovm_inst_t inst, char *s, unsigned n)
{
  char *q;
  unsigned k, kk;

  for (q = s, k = n;;)
    {
      OVM_ASSERT (k > 0);
      if (*q == '<')
	break;
      ++q;
      --k;
    }

  kk = n - k;
  OVM_ASSERT (k >= 9 && strncmp (q, "</String>", 9) == 0);
  _whitespace_skip (&q, &k);
  OVM_ASSERT (k == 0);

  {
    char buf[kk + 1], c;

    for (q = buf; kk;)
      {
	c = *s;
	if (c == '&')
	  {
	    if (kk >= 6 && strncmp (s, "&quot;", 6) == 0)
	      {
		c = '"';
		s += 6;
		kk -= 6;
	      }
	    else if (kk >= 6 && strncmp (s, "&apos;", 6) == 0)
	      {
		c = '\'';
		s += 6;
		kk -= 6;
	      }
	    else if (kk >= 5 && strncmp (s, "&amp;", 5) == 0)
	      {
		c = '&';
		s += 5;
		kk -= 5;
	      }
	    else if (kk >= 4 && strncmp (s, "&lt;", 4) == 0)
	      {
		c = '<';
		s += 4;
		kk -= 4;
	      }
	    else if (kk >= 4 && strncmp (s, "&gt;", 4) == 0)
	      {
		c = '>';
		s += 4;
		kk -= 4;
	      }
	    else
	      {
		OVM_ASSERT (0);
	      }
	  }
	else
	  {
	    ++s;
	    --kk;
	  }

	*q = c;
	++q;
      }

    __ovm_strval_initc (ovm, STRVAL (inst), 1, q - buf, q);
  }
}

static void
_xml_parse_string (ovm_t ovm, ovm_inst_t inst, char *s, unsigned n)
{
  _whitespace_skip (&s, &n);
  OVM_ASSERT (n >= 8 && strncmp (s, "<String>", 8) == 0);
  _xml_parse_string2 (ovm, inst, s + 8, n - 8);
}

static void
_ovm_string_init (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst, unsigned argc,
		  ovm_inst_t * argv)
{
  if (argc > 0)
    {
      ovm_inst_t arg = argv[0];
      ovm_class_t arg_cl = ovm_inst_of (arg);

      if (arg == OVM_NIL)
	{
	  __ovm_strval_initc (ovm, STRVAL (inst), 1, 5, "#nil");
	}
      else if (arg_cl == ovm_cl_bool)
	{
	  char *s;
	  unsigned n;

	  if (BOOLVAL (arg))
	    {
	      s = "#true";
	      n = 6;
	    }
	  else
	    {
	      s = "#false";
	      n = 7;
	    }

	  __ovm_strval_initc (ovm, STRVAL (inst), 1, n, s);
	}
      else if (arg_cl == ovm_cl_integer)
	{
	  char buf[32];

	  snprintf (buf, sizeof (buf), "%lld", INTVAL (arg));
	  __ovm_strval_initc (ovm, STRVAL (inst), 1, strlen (buf) + 1, buf);
	}
      else if (arg_cl == ovm_cl_float)
	{
	  char buf[64];

	  snprintf (buf, sizeof (buf), "%Lg", FLOATVAL (arg));
	  __ovm_strval_initc (ovm, STRVAL (inst), 1, strlen (buf) + 1, buf);
	}
      else if (arg_cl == ovm_cl_xml)
	{
	  _xml_parse_string (ovm, inst, STRVAL (arg)->data,
			     STRVAL (arg)->size - 1);
	}
      else if (arg_cl == ovm_cl_bmap)
	{
	  unsigned k =
	    2 + BMVAL (arg)->size + (BMVAL (arg)->size - 1) / 4 + 1, n, nn, i;
	  char *buf = 0, *q;
	  ovm_bmval_unit_t *p, u;

	  _ovm_malloc (ovm, k, (void **) &buf);

	  for (*(q = &buf[k - 1]) = 0, p = BMVAL (arg)->data, i = 0, n =
	       BMVAL (arg)->size; n; ++p)
	    {
	      u = *p;

	      for (nn = OVM_BMVAL_UNIT_BITS; nn && n; --nn, --n, ++i, u >>= 1)
		{
		  *--q = '0' + (u & 1);
		  if (n > 1 && (i & 3) == 3)
		    *--q = '_';
		}
	    }
	  *--q = 'b';
	  *--q = '0';

	  STRVAL (inst)->size = k;
	  STRVAL (inst)->data = buf;
	}
      else if (arg_cl == ovm_cl_ref)
	{
	  char buf[2 + 18 + 1];

	  snprintf (buf, sizeof (buf), "#@%p", REFVAL (arg));
	  __ovm_strval_initc (ovm, STRVAL (inst), 1, strlen (buf), buf);
	}
      else if (arg_cl == ovm_cl_pair)
	{
	  OVM_FRAME_DECL (fr, work[5]);

	  OVM_FRAME_ENTER (ovm, fr);

	  __ovm_string_newc (ovm, &fr->work[0], "<");
	  OVM_INST_NEW (ovm, fr->work[1], ovm_cl_string, CAR (arg));
	  __ovm_string_newc (ovm, &fr->work[2], ", ");
	  OVM_INST_NEW (ovm, fr->work[3], ovm_cl_string, CDR (arg));
	  __ovm_string_newc (ovm, &fr->work[4], ">");

	  __ovm_strval_inita (ovm, STRVAL (inst), 5, fr->work);

	  OVM_FRAME_LEAVE (ovm);
	}
      else if (arg_cl == ovm_cl_list)
	{
	  unsigned n = __ovm_list_len (arg);
	  unsigned nn = 2 + (n > 0 ? 2 * n - 1 : n);

	  {
	    ovm_inst_t *q;
	    unsigned i;
	    OVM_FRAME_DECL (fr, delim, s[nn]);

	    OVM_FRAME_ENTER (ovm, fr);

	    __ovm_string_newc (ovm, &fr->delim, ", ");

	    q = fr->s;
	    __ovm_string_newc (ovm, q, "(");
	    ++q;

	    for (i = 0; arg; arg = CDR (arg), ++i)
	      {
		if (i > 0)
		  {
		    _ovm_assign (ovm, q, fr->delim);
		    ++q;
		  }
		_ovm_inst_new (ovm, q, ovm_cl_string, 1, CAR (arg));
		++q;
	      }

	    __ovm_string_newc (ovm, q, ")");

	    __ovm_strval_inita (ovm, STRVAL (inst), nn, fr->s);

	    OVM_FRAME_LEAVE (ovm);
	  }
	}
      else if (arg_cl == ovm_cl_array)
	{
	  unsigned n = ARRAYVAL (arg)->size;
	  unsigned nn = 2 + (n > 0 ? 2 * n - 1 : n);

	  {
	    ovm_inst_t *q, *p;
	    unsigned i;
	    OVM_FRAME_DECL (fr, delim, s[nn]);

	    OVM_FRAME_ENTER (ovm, fr);

	    __ovm_string_newc (ovm, &fr->delim, ", ");

	    q = fr->s;
	    __ovm_string_newc (ovm, q, "[");
	    ++q;

	    for (p = ARRAYVAL (arg)->data, i = 0; n; --n, ++i, ++p)
	      {
		if (i > 0)
		  {
		    _ovm_assign (ovm, q, fr->delim);
		    ++q;
		  }
		_ovm_inst_new (ovm, q, ovm_cl_string, 1, *p);
		++q;
	      }

	    __ovm_string_newc (ovm, q, "]");

	    __ovm_strval_inita (ovm, STRVAL (inst), nn, fr->s);

	    OVM_FRAME_LEAVE (ovm);
	  }
	}
      else if (arg_cl == ovm_cl_set)
	{
	  unsigned n = SETVAL (arg)->cnt;
	  unsigned nn = 2 + (n > 0 ? 2 * n - 1 : n);

	  {
	    ovm_inst_t *q, *p, r;
	    unsigned i;
	    OVM_FRAME_DECL (fr, delim, s[nn]);

	    OVM_FRAME_ENTER (ovm, fr);

	    __ovm_string_newc (ovm, &fr->delim, ", ");

	    q = fr->s;
	    __ovm_string_newc (ovm, q, "{");
	    ++q;

	    for (p = SETVAL (arg)->base->data, i = 0, n =
		 SETVAL (arg)->base->size; n; --n, ++p)
	      {
		for (r = *p; r; r = CDR (r), ++i)
		  {
		    if (i > 0)
		      {
			_ovm_assign (ovm, q, fr->delim);
			++q;
		      }
		    _ovm_inst_new (ovm, q, ovm_cl_string, 1, CAR (r));
		    ++q;
		  }
	      }

	    __ovm_string_newc (ovm, q, "}");

	    __ovm_strval_inita (ovm, STRVAL (inst), nn, fr->s);

	    OVM_FRAME_LEAVE (ovm);
	  }
	}
      else if (arg_cl == ovm_cl_dict)
	{
	  unsigned n = DICTVAL (arg)->base->cnt;
	  unsigned nn = 2 + (n > 0 ? 4 * n - 1 : n);

	  {
	    ovm_inst_t *q, *p, r;
	    unsigned i;
	    OVM_FRAME_DECL (fr, delim[2], s[nn]);

	    OVM_FRAME_ENTER (ovm, fr);

	    __ovm_string_newc (ovm, &fr->delim[0], ", ");
	    __ovm_string_newc (ovm, &fr->delim[1], ": ");

	    q = fr->s;
	    __ovm_string_newc (ovm, q, "{");
	    ++q;

	    for (p = DICTVAL (arg)->base->base->data, i = 0, n =
		 DICTVAL (arg)->base->base->size; n; --n, ++p)
	      {
		for (r = *p; r; r = CDR (r), ++i)
		  {
		    if (i > 0)
		      {
			_ovm_assign (ovm, q, fr->delim[0]);
			++q;
		      }

		    _ovm_inst_new (ovm, q, ovm_cl_string, 1, CAR (CAR (r)));
		    ++q;
		    _ovm_assign (ovm, q, fr->delim[1]);
		    ++q;
		    _ovm_inst_new (ovm, q, ovm_cl_string, 1, CDR (CAR (r)));
		    ++q;
		  }
	      }

	    __ovm_string_newc (ovm, q, "}");

	    __ovm_strval_inita (ovm, STRVAL (inst), nn, fr->s);

	    OVM_FRAME_LEAVE (ovm);
	  }
	}
      else
	{
	  OVM_ASSERT (0);
	}

      --argc;
      ++argv;
    }

  _ovm_init_parent (ovm, cl, inst, argc, argv);
}

static void
_ovm_string_free (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst)
{
  _ovm_free (ovm, (void **) &STRVAL (inst)->data, STRVAL (inst)->size);

  _ovm_free_parent (ovm, cl, inst);
}

static void
_ovm_string_at_len (ovm_t ovm, ovm_inst_t * dst, unsigned argc,
		    ovm_inst_t * argv)
{
  ovm_intval_t ofs, len;
  struct ovm_strval sv[1];

  OVM_ASSERT (argc == 2);
  OVM_ASSERT (ovm_is_kind_of (argv[0], ovm_cl_string));
  OVM_ASSERT (ovm_is_kind_of (argv[1], ovm_cl_integer));
  ofs = INTVAL (argv[1]);
  OVM_ASSERT (ovm_is_kind_of (argv[2], ovm_cl_integer));
  len = INTVAL (argv[2]);

  _slice (&ofs, &len, STRVAL (argv[0])->size - 1);

  __ovm_strval_new (ovm, dst,
		    __ovm_strval_initc (ovm, sv, 1, len + 1,
					STRVAL (argv[0])->data + ofs));
}

struct ovm_class ovm_cl_string[1] = { {
				       .name = "String",
				       .parent = ovm_cl_object,
				       .new = _ovm_inst_new1,
				       .init = _ovm_string_init,
				       .walk = _ovm_walk_parent,
				       .free = _ovm_string_free,
				       .inst_method_func_tbl = {
								[OVM_INST_METHOD_SEL_AT_LEN] = _ovm_string_at_len}
				       }
};

/***************************************************************************/

void
_ovm_xml_newc (ovm_t ovm, ovm_inst_t * dst, char *s)
{
  _ovm_dst_chk (ovm, dst);

  _ovm_inst_alloc (ovm, ovm_cl_xml, dst);

  __ovm_strval_initc (ovm, STRVAL (*dst), 1, strlen (s) + 1, s);
}

static void
_ovm_xml_init (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst, unsigned argc,
	       ovm_inst_t * argv)
{
  if (argc > 0)
    {
      ovm_inst_t arg = argv[0];
      ovm_class_t arg_cl = ovm_inst_of (arg);

      if (arg_cl == ovm_cl_bool)
	{
	  char *buf = 0;
	  unsigned n = 9 + 1 + 10 + 1;

	  _ovm_malloc (ovm, n, (void **) &buf);

	  strcpy (buf, "<Boolean>");
	  buf[9] = BOOLVAL (arg) ? 'T' : 'F';
	  strcpy (buf + 10, "</Boolean>");

	  STRVAL (inst)->size = n;
	  STRVAL (inst)->data = buf;
	}
      else if (arg_cl == ovm_cl_integer)
	{
	  char buf[9 + 32 + 10 + 1];
	  unsigned n;

	  strcpy (buf, "<Integer>");
	  snprintf (buf + 9, sizeof (buf) - 9, "%lld", INTVAL (arg));
	  n = strlen (buf);
	  strcpy (buf + n, "</Integer>");

	  STRVAL (inst)->size = n + 10 + 1;
	  _ovm_malloc (ovm, STRVAL (inst)->size,
		       (void **) &STRVAL (inst)->data);
	  memcpy (STRVAL (inst)->data, buf, STRVAL (inst)->size);
	}
      else if (arg_cl == ovm_cl_float)
	{
	  char buf[7 + 64 + 8 + 1];
	  unsigned n;

	  strcpy (buf, "<Float>");
	  snprintf (buf + 7, sizeof (buf) - 7, "%Lg", FLOATVAL (arg));
	  n = strlen (buf);
	  strcpy (buf + n, "</Float>");

	  STRVAL (inst)->size = n + 8 + 1;
	  _ovm_malloc (ovm, STRVAL (inst)->size,
		       (void **) &STRVAL (inst)->data);
	  memcpy (STRVAL (inst)->data, buf, STRVAL (inst)->size);
	}
      else if (arg_cl == ovm_cl_string)
	{
	  unsigned n, k;
	  char *buf = 0, *p, *q, c;

	  for (k = 8, p = STRVAL (arg)->data, n = STRVAL (arg)->size - 1; n;
	       --n, ++p)
	    {
	      switch (*p)
		{
		case '"':
		case '\'':
		  k += 6;
		  break;
		case '&':
		  k += 5;
		  break;
		case '<':
		case '>':
		  k += 4;
		  break;
		default:
		  ++k;
		}
	    }
	  k += 9 + 1;

	  _ovm_malloc (ovm, k, (void **) &buf);

	  q = buf;
	  strcpy (q, "<String>");
	  q += 8;

	  for (p = STRVAL (arg)->data, n = STRVAL (arg)->size - 1;
	       n; --n, ++p)
	    {
	      c = *p;
	      switch (c)
		{
		case '"':
		  strcpy (q, "&quot;");
		  q += 6;
		  break;
		case '\'':
		  strcpy (q, "&apos;");
		  q += 6;
		  break;
		case '&':
		  strcpy (q, "&amp;");
		  q += 5;
		  break;
		case '<':
		  strcpy (q, "&lt;");
		  q += 4;
		  break;
		case '>':
		  strcpy (q, "&gt;");
		  q += 4;
		  break;
		default:
		  *q++ = c;
		}
	    }
	  strcpy (q, "</String>");

	  STRVAL (inst)->size = k;
	  STRVAL (inst)->data = buf;
	}
      else if (arg_cl == ovm_cl_bmap)
	{
	  unsigned n = bmap_bits_to_units (BMVAL (arg)->size), nn;
	  unsigned k = 8 + 2 * bmap_units_to_bytes (n) + 9 + 1;
	  ovm_bmval_unit_t *p, u;
	  char *buf = 0, *q;

	  _ovm_malloc (ovm, k, (void **) &buf);

	  q = buf;
	  strcpy (q, "<Bitmap>");
	  q += 8;

	  for (p = BMVAL (arg)->data; n; --n, ++p)
	    {
	      for (u = *p, nn = bmap_units_to_bytes (1); nn; --nn, u >>= 8)
		{
		  sprintf (q, "%02x", u & 0xff);
		  q += 2;
		}
	    }

	  strcpy (q, "</Bitmap>");

	  STRVAL (inst)->size = k;
	  STRVAL (inst)->data = buf;
	}
      else if (arg_cl == ovm_cl_pair)
	{
	  OVM_FRAME_DECL (fr, work[4]);

	  OVM_FRAME_ENTER (ovm, fr);

	  __ovm_string_newc (ovm, &fr->work[0], "<Pair>");
	  OVM_INST_NEW (ovm, fr->work[1], ovm_cl_xml, CAR (arg));
	  OVM_INST_NEW (ovm, fr->work[2], ovm_cl_xml, CDR (arg));
	  __ovm_string_newc (ovm, &fr->work[3], "</Pair>");

	  __ovm_strval_inita (ovm, STRVAL (inst), 4, fr->work);

	  OVM_FRAME_LEAVE (ovm);
	}
      else if (arg_cl == ovm_cl_list)
	{
	  unsigned n = __ovm_list_len (arg);
	  unsigned nn = 2 + n;

	  {
	    ovm_inst_t *q;

	    OVM_FRAME_DECL (fr, s[nn]);

	    OVM_FRAME_ENTER (ovm, fr);

	    q = fr->s;
	    _ovm_string_newc (ovm, q, "<List>");
	    ++q;

	    for (; arg; arg = CDR (arg))
	      {
		_ovm_inst_new (ovm, q, ovm_cl_xml, 1, CAR (arg));
		++q;
	      }

	    _ovm_string_newc (ovm, q, "</List>");

	    __ovm_strval_inita (ovm, STRVAL (inst), nn, fr->s);

	    OVM_FRAME_LEAVE (ovm);
	  }
	}
      else if (arg_cl == ovm_cl_array)
	{
	  unsigned n = ARRAYVAL (arg)->size;
	  unsigned nn = 2 + n;

	  {
	    ovm_inst_t *q, *p;
	    OVM_FRAME_DECL (fr, s[nn]);

	    OVM_FRAME_ENTER (ovm, fr);

	    q = fr->s;
	    _ovm_string_newc (ovm, q, "<Array>");
	    ++q;

	    for (p = ARRAYVAL (arg)->data; n; --n, ++p)
	      {
		_ovm_inst_new (ovm, q, ovm_cl_xml, 1, *p);
		++q;
	      }

	    _ovm_string_newc (ovm, q, "</Array");

	    __ovm_strval_inita (ovm, STRVAL (inst), nn, fr->s);

	    OVM_FRAME_LEAVE (ovm);
	  }
	}
      else if (arg_cl == ovm_cl_set)
	{
	  unsigned n = SETVAL (arg)->cnt;
	  unsigned nn = 2 + n;

	  {
	    ovm_inst_t *q, *p, r;
	    OVM_FRAME_DECL (fr, s[nn]);

	    OVM_FRAME_ENTER (ovm, fr);

	    q = fr->s;
	    _ovm_string_newc (ovm, q, "<Set>");
	    ++q;

	    for (p = SETVAL (arg)->base->data, n = SETVAL (arg)->base->size;
		 n; --n, ++p)
	      {
		for (r = *p; r; r = CDR (r))
		  {
		    _ovm_inst_new (ovm, q, ovm_cl_string, 1, CAR (r));
		    ++q;
		  }
	      }

	    _ovm_string_newc (ovm, q, "</Set>");

	    __ovm_strval_inita (ovm, STRVAL (inst), nn, fr->s);

	    OVM_FRAME_LEAVE (ovm);
	  }
	}
      else if (arg_cl == ovm_cl_dict)
	{
	  unsigned n = DICTVAL (arg)->base->cnt;
	  unsigned nn = 2 + n;

	  {
	    ovm_inst_t *q, *p, r;
	    OVM_FRAME_DECL (fr, s[nn]);

	    OVM_FRAME_ENTER (ovm, fr);

	    q = fr->s;
	    _ovm_string_newc (ovm, q, "<Dictionary>");
	    ++q;

	    for (p = DICTVAL (arg)->base->base->data, n =
		 DICTVAL (arg)->base->base->size; n; --n, ++p)
	      {
		for (r = *p; r; r = CDR (r))
		  {
		    _ovm_inst_new (ovm, q, ovm_cl_string, 1, CAR (r));
		    ++q;
		  }
	      }

	    _ovm_string_newc (ovm, q, "</Dictionary>");

	    __ovm_strval_inita (ovm, STRVAL (inst), nn, fr->s);

	    OVM_FRAME_LEAVE (ovm);
	  }
	}
      else
	{
	  OVM_ASSERT (0);
	}

      --argc;
      ++argv;
    }

  _ovm_init_parent (ovm, cl, inst, argc, argv);
}

static void
__xml_parse(ovm_t ovm, ovm_inst_t *dst, char *s, unsigned n)
{
  OVM_FRAME_DECL(fr, work);

  _whitespace_skip(&s, &n);

  OVM_FRAME_ENTER(ovm, fr);

  if (n >= 9 && strncmp (s, "<Boolean>", 9) == 0) {
    _ovm_inst_alloc(ovm, ovm_cl_bool, &fr->work);
    _xml_parse_bool2 (ovm, fr->work, s + 9, n - 9);
  } else if (n >= 9 && strncmp (s, "<Integer>", 9) == 0) {
    _ovm_inst_alloc(ovm, ovm_cl_integer, &fr->work);
    _xml_parse_int2 (ovm, fr->work, s + 9, n - 9);
  } else if (n >= 7 && strncmp (s, "<Float>", 7) == 0) {
    _ovm_inst_alloc(ovm, ovm_cl_float, &fr->work);
    _xml_parse_float2 (ovm, fr->work, s + 7, n - 7);
  } else if (n >= 8 && strncmp (s, "<String>", 8) == 0) {
    _ovm_inst_alloc(ovm, ovm_cl_string, &fr->work);
    _xml_parse_string2 (ovm, fr->work, s + 8, n - 8);
#if 0
  } else if (n >= 8 && strncmp (s, "<Bitmap>", 8) == 0) {
    _ovm_inst_alloc(ovm, ovm_cl_bmap, &fr->work);
    _xml_parse_bitmap2 (ovm, fr->work, s + 8, n - 8);
  } else if (n >= 6 && strncmp (s, "<Pair>", 6) == 0) {
    _ovm_inst_alloc(ovm, ovm_cl_pair, &fr->work);
    _xml_parse_pair2 (ovm, fr->work, s + 6, n - 6);
  } else if (n >= 6 && strncmp (s, "<List>", 6) == 0) {
    _ovm_inst_alloc(ovm, ovm_cl_list, &fr->work);
    _xml_parse_list2 (ovm, fr->work, s + 6, n - 6);
  } else if (n >= 7 && strncmp (s, "<Array>", 7) == 0) {
    _ovm_inst_alloc(ovm, ovm_cl_array, &fr->work);
    _xml_parse_array2 (ovm, fr->work, s + 7, n - 7);
  } else if (n >= 5 && strncmp (s, "<Set>", 5) == 0) {
    _ovm_inst_alloc(ovm, ovm_cl_set, &fr->work);
    _xml_parse_set2 (ovm, fr->work, s + 5, n - 5);
  } else if (n >= 12 && strncmp (s, "<Dictionary>", 12) == 0) {
    _ovm_inst_alloc(ovm, ovm_cl_dict, &fr->work);
    _xml_parse_dict2 (ovm, fr->work, s + 12, n - 12);
#endif
  } else {
    OVM_ASSERT(0);
  }
  
  _ovm_assign(ovm, dst, fr->work);

  OVM_FRAME_LEAVE(ovm);
}

static void
_ovm_xml_parse(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  OVM_ASSERT(argc == 0);
  OVM_ASSERT(ovm_is_kind_of(argv[0], ovm_cl_xml));

  __xml_parse(ovm, dst, STRVAL(argv[0])->data, STRVAL(argv[0])->size - 1);
}

struct ovm_class ovm_cl_xml[1] = { {
				    .name = "Xml",
				    .parent = ovm_cl_string,
				    .new = _ovm_inst_new1,
				    .init = _ovm_xml_init,
				    .walk = _ovm_walk_parent,
				    .free = _ovm_free_parent,
				    .inst_method_func_tbl = {
      [OVM_INST_METHOD_SEL_PARSE] = _ovm_xml_parse
							     }
				    }
};

/***************************************************************************/

static unsigned
__ovm_bmap_init (ovm_t ovm, ovm_inst_t inst, unsigned size)
{
  unsigned n;

  OVM_ASSERT (size > 0);

  BMVAL (inst)->size = size;
  n = bmap_units_to_bytes (bmap_bits_to_units (size));
  _ovm_malloc (ovm, n, (void **) &BMVAL (inst)->data);

  return (n);
}

static void
__ovm_bmap_new (ovm_t ovm, ovm_inst_t * dst, unsigned size)
{
  _ovm_inst_alloc (ovm, ovm_cl_bmap, dst);

  __ovm_bmap_init (ovm, *dst, size);
}

void
ovm_bmap_newc (ovm_t ovm, ovm_inst_t * dst, unsigned size)
{
  _ovm_inst_alloc (ovm, ovm_cl_bmap, dst);

  memset (BMVAL (*dst)->data, 0, __ovm_bmap_init (ovm, *dst, size));
}

static void
__ovm_bmap_copy (ovm_t ovm, ovm_inst_t to, ovm_inst_t from)
{
  memcpy (BMVAL (to)->data, BMVAL (from)->data,
	  __ovm_bmap_init (ovm, to, BMVAL (from)->size));
}

static void
_ovm_bmap_copy (ovm_t ovm, ovm_inst_t * dst, ovm_inst_t src)
{
  _ovm_inst_alloc (ovm, ovm_cl_bmap, dst);

  __ovm_bmap_copy (ovm, *dst, src);
}

static void
_xml_parse_bmap2(ovm_t ovm, ovm_inst_t inst, char *s, unsigned n)
{
  _whitespace_skip(&s, &n);

  
}

static void
_xml_parse_bmap(ovm_t ovm, ovm_inst_t inst, char *s, unsigned n)
{
  _whitespace_skip(&s, &n);
  OVM_ASSERT(n >= 8 && strncmp(s, "<Bitmap>", 8) == 0);
  _xml_parse_bmap2(ovm, inst, s + 8, n - 8);
}

static void
_ovm_bmap_init (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst,
		unsigned argc, ovm_inst_t * argv)
{
  ovm_inst_t arg;
  ovm_class_t arg_cl;
  unsigned n;

  OVM_ASSERT (argc >= 1);

  arg = argv[0];
  arg_cl = ovm_inst_of (arg);

  if (arg_cl == ovm_cl_integer)
    {
      OVM_ASSERT (INTVAL (arg) >= 0);

      memset (BMVAL (inst)->data, 0,
	      __ovm_bmap_init (ovm, inst, INTVAL (arg)));

    }
  else if (arg_cl == ovm_cl_bmap)
    {
      __ovm_bmap_copy (ovm, inst, arg);
    }
  else if (arg_cl == ovm_cl_xml) {
    _xml_parse_bmap(ovm, inst, STRVAL(arg)->data, STRVAL(arg)->size - 1);
  } else
    {
      OVM_ASSERT (0);
    }

  ++argv;
  --argc;

  _ovm_init_parent (ovm, cl, inst, argc, argv);
}

static void
_ovm_bmap_free (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst)
{
  _ovm_free (ovm, (void **) &BMVAL (inst)->data,
	     bmap_units_to_bytes (bmap_bits_to_units (BMVAL (inst)->size)));

  _ovm_free_parent (ovm, cl, inst);
}

static void
__ovm_bmap_get (ovm_bmval_unit_t * to, ovm_bmval_unit_t * from, unsigned ofs,
		unsigned len)
{
  unsigned sh, rsh, n, k;
  ovm_bmval_unit_t *p, *q;

  for (sh = bmap_unit_sh (ofs), rsh = OVM_BMVAL_UNIT_BITS - sh, q = to, p =
       from + bmap_unit_idx (ofs), n = len; n; n -= k, ++p, ++q)
    {
      k = (n >= OVM_BMVAL_UNIT_BITS) ? OVM_BMVAL_UNIT_BITS : n;

      *q = p[0] >> sh;
      if (k > rsh)
	*q |= p[1] << rsh;
    }
  if (k < OVM_BMVAL_UNIT_BITS)
    q[-1] &= ~bits (k);
}

static void
_ovm_bmap_at_len (ovm_t ovm, ovm_inst_t * dst, unsigned argc,
		  ovm_inst_t * argv)
{
  ovm_intval_t ofs, len;

  OVM_FRAME_DECL (fr, work);

  OVM_ASSERT (argc == 2);
  OVM_ASSERT (ovm_is_kind_of (argv[0], ovm_cl_bmap));
  OVM_ASSERT (ovm_is_kind_of (argv[1], ovm_cl_integer));
  ofs = INTVAL (argv[1]);
  OVM_ASSERT (ovm_is_kind_of (argv[2], ovm_cl_integer));
  len = INTVAL (argv[2]);

  _slice (&ofs, &len, BMVAL (argv[0])->size);

  OVM_FRAME_ENTER (ovm, fr);

  __ovm_bmap_new (ovm, &fr->work, len);

  __ovm_bmap_get (BMVAL (fr->work)->data, BMVAL (argv[0])->data, ofs, len);

  _ovm_assign (ovm, dst, fr->work);

  OVM_FRAME_LEAVE (ovm);
}

static void
__ovm_bmap_set (ovm_bmval_unit_t * to, unsigned ofs, ovm_bmval_unit_t * from,
		unsigned len)
{
  unsigned sh, rsh, n, k;
  ovm_bmval_unit_t *p, *q, m, u;

  for (sh = bmap_unit_sh (ofs), rsh = OVM_BMVAL_UNIT_BITS - sh, q =
       to + bmap_unit_idx (ofs), p = from, n = len; n; n -= k, ++p, ++q)
    {
      k = (n >= OVM_BMVAL_UNIT_BITS) ? OVM_BMVAL_UNIT_BITS : n;
      m = bits (k);
      u = *p & m;

      q[0] = (q[0] & ~(m << sh)) | (u << sh);
      if (k > rsh)
	q[1] = (q[1] & ~(m >> rsh)) | (u >> rsh);
    }
}

static void
_ovm_bmap_at_put (ovm_t ovm, ovm_inst_t * dst, unsigned argc,
		  ovm_inst_t * argv)
{
  ovm_intval_t ofs, len;

  OVM_FRAME_DECL (fr, work);

  OVM_ASSERT (argc == 2);
  OVM_ASSERT (ovm_is_kind_of (argv[0], ovm_cl_bmap));
  OVM_ASSERT (ovm_is_kind_of (argv[1], ovm_cl_integer));
  ofs = INTVAL (argv[1]);
  OVM_ASSERT (ovm_is_kind_of (argv[2], ovm_cl_bmap));
  len = BMVAL (argv[2])->size;

  _slice (&ofs, &len, BMVAL (argv[0])->size);

  OVM_FRAME_ENTER (ovm, fr);

  _ovm_bmap_copy (ovm, &fr->work, argv[0]);

  __ovm_bmap_set (BMVAL (fr->work)->data, ofs, BMVAL (argv[2])->data, len);

  _ovm_assign (ovm, dst, fr->work);

  OVM_FRAME_LEAVE (ovm);
}

static void
_ovm_bmap_at_len_put (ovm_t ovm, ovm_inst_t * dst, unsigned argc,
		      ovm_inst_t * argv)
{
  ovm_intval_t ofs, len;

  OVM_FRAME_DECL (fr, work);

  OVM_ASSERT (argc == 3);
  OVM_ASSERT (ovm_is_kind_of (argv[0], ovm_cl_bmap));
  OVM_ASSERT (ovm_is_kind_of (argv[1], ovm_cl_integer));
  ofs = INTVAL (argv[1]);
  OVM_ASSERT (ovm_is_kind_of (argv[2], ovm_cl_integer));
  len = INTVAL (argv[2]);
  OVM_ASSERT (ovm_is_kind_of (argv[3], ovm_cl_integer));

  _slice (&ofs, &len, BMVAL (argv[0])->size);

  OVM_ASSERT (len <= (8 * sizeof (INTVAL (argv[3]))));

  OVM_FRAME_ENTER (ovm, fr);

  _ovm_bmap_copy (ovm, &fr->work, argv[0]);

  __ovm_bmap_set (BMVAL (fr->work)->data, ofs,
		  (ovm_bmval_unit_t *) & INTVAL (argv[3]), len);

  _ovm_assign (ovm, dst, fr->work);

  OVM_FRAME_LEAVE (ovm);
}

struct ovm_class ovm_cl_bmap[1] = { {
				     .name = "Bitmap",
				     .parent = ovm_cl_object,
				     .new = _ovm_inst_new1,
				     .init = _ovm_bmap_init,
				     .walk = _ovm_walk_parent,
				     .free = _ovm_bmap_free,
				     .inst_method_func_tbl = {
							      [OVM_INST_METHOD_SEL_AT_LEN] = _ovm_bmap_at_len,
							      [OVM_INST_METHOD_SEL_AT_LEN_PUT] = _ovm_bmap_at_len_put,
							      [OVM_INST_METHOD_SEL_AT_PUT] = _ovm_bmap_at_put}
				     }
};

/***************************************************************************/

static void
_ovm_dptr_init (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst,
		unsigned argc, ovm_inst_t * argv)
{
  OVM_ASSERT (argc >= 2);

  OVM_ASSIGN (ovm, CAR (inst), argv[0]);
  OVM_ASSIGN (ovm, CDR (inst), argv[1]);

  argc -= 2;
  argv += 2;

  _ovm_init_parent (ovm, cl, inst, argc, argv);
}

static void
_ovm_dptr_walk (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst,
		void (*func) (struct ovm *, ovm_inst_t))
{
  (*func) (ovm, CAR (inst));
  (*func) (ovm, CDR (inst));

  _ovm_walk_parent (ovm, cl, inst, func);
}

static void
_ovm_dptr_car (ovm_t ovm, ovm_inst_t * dst, unsigned argc, ovm_inst_t * argv)
{
  OVM_ASSERT (argc == 0);
  OVM_ASSERT (ovm_is_kind_of (argv[0], ovm_cl_dptr));

  _ovm_assign (ovm, dst, CAR (argv[0]));
}

static void
_ovm_dptr_cdr (ovm_t ovm, ovm_inst_t * dst, unsigned argc, ovm_inst_t * argv)
{
  OVM_ASSERT (argc == 0);
  OVM_ASSERT (ovm_is_kind_of (argv[0], ovm_cl_dptr));

  _ovm_assign (ovm, dst, CDR (argv[0]));
}

static void
_ovm_dptr_hash (ovm_t ovm, ovm_inst_t * dst, unsigned argc, ovm_inst_t * argv)
{
  unsigned h;

  OVM_FRAME_DECL (fr, work);

  OVM_ASSERT (argc == 0);
  OVM_ASSERT (ovm_is_kind_of (argv[0], ovm_cl_dptr));

  OVM_FRAME_ENTER (ovm, fr);

  OVM_INST_METHOD_CALL (ovm, fr->work, CAR (argv[0]),
			OVM_INST_METHOD_SEL_HASH);
  h = INTVAL (fr->work);

  OVM_INST_METHOD_CALL (ovm, fr->work, CDR (argv[0]),
			OVM_INST_METHOD_SEL_HASH);

  __ovm_integer_newc (ovm, dst, h + INTVAL (fr->work));

  OVM_FRAME_LEAVE (ovm);
}

struct ovm_class ovm_cl_dptr[1] = { {
				     .name = "Dptr",
				     .parent = ovm_cl_object,
				     /* No .new - not instantiable */
				     .init = _ovm_dptr_init,
				     .walk = _ovm_dptr_walk,
				     .free = _ovm_free_parent,
				     .inst_method_func_tbl = {
							      [OVM_INST_METHOD_SEL_CAR] = _ovm_dptr_car,
							      [OVM_INST_METHOD_SEL_CDR] = _ovm_dptr_cdr,
							      [OVM_INST_METHOD_SEL_HASH] = _ovm_dptr_hash}
				     }
};

/***************************************************************************/

struct ovm_class ovm_cl_pair[1] = { {
				     .name = "Pair",
				     .parent = ovm_cl_dptr,
				     .new = _ovm_inst_new1,
				     .init = _ovm_init_parent,
				     .walk = _ovm_walk_parent,
				     .free = _ovm_free_parent,
				     .inst_method_func_tbl = {
							      }
				     }
};

/***************************************************************************/

static unsigned
__ovm_list_len (ovm_inst_t inst)
{
  unsigned result;

  for (result = 0; inst; inst = CDR (inst))
    ++result;

  return (result);
}

static void
_ovm_list_init (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst,
		unsigned argc, ovm_inst_t * argv)
{
  OVM_ASSERT (argc < 2 || argv[1] == OVM_NIL
	      || ovm_is_kind_of (argv[1], ovm_cl_list));

  _ovm_init_parent (ovm, cl, inst, argc, argv);
}

static void
_ovm_list_hash (ovm_t ovm, ovm_inst_t * dst, unsigned argc, ovm_inst_t * argv)
{
  unsigned h;
  ovm_inst_t p;

  OVM_FRAME_DECL (fr, work);

  OVM_ASSERT (argc == 0);
  OVM_ASSERT (ovm_is_kind_of (argv[0], ovm_cl_dptr));

  OVM_FRAME_ENTER (ovm, fr);

  for (h = 0, p = argv[0]; p; p = CDR (p))
    {
      OVM_INST_METHOD_CALL (ovm, fr->work, CAR (p), OVM_INST_METHOD_SEL_HASH);
      h += INTVAL (fr->work);
    }

  __ovm_integer_newc (ovm, dst, h);

  OVM_FRAME_LEAVE (ovm);
}

struct ovm_class ovm_cl_list[1] = { {
				     .name = "List",
				     .parent = ovm_cl_dptr,
				     .new = _ovm_inst_new1,
				     .init = _ovm_list_init,
				     .walk = _ovm_walk_parent,
				     .free = _ovm_free_parent,
				     .inst_method_func_tbl = {
							      [OVM_INST_METHOD_SEL_HASH] = _ovm_list_hash}
				     }
};

/***************************************************************************/

static inline unsigned
__ovm_array_size_bytes (unsigned size)
{
  return (size * sizeof (ARRAYVAL (OVM_NIL)->data[0]));
}

static void
__ovm_array_init (ovm_t ovm, ovm_inst_t inst, unsigned size)
{
  ARRAYVAL (inst)->size = size;
  _ovm_zmalloc (ovm, __ovm_array_size_bytes (size),
		(void **) &ARRAYVAL (inst)->data);
}

static void
_ovm_array_copy (ovm_t ovm, ovm_inst_t * dst, ovm_inst_t src)
{
  unsigned n;
  ovm_inst_t *p, *q;

  OVM_FRAME_DECL (fr, work);

  OVM_FRAME_ENTER (ovm, fr);

  _ovm_inst_alloc (ovm, ovm_inst_of (src), &fr->work);

  __ovm_array_init (ovm, fr->work, ARRAYVAL (src)->size);

  for (p = ARRAYVAL (fr->work)->data, q =
       ARRAYVAL (src)->data, n = ARRAYVAL (src)->size; n; --n, ++p, ++q)
    {
      _ovm_assign (ovm, p, *q);
    }

  _ovm_assign (ovm, dst, fr->work);

  OVM_FRAME_LEAVE (ovm);
}

void
_ovm_array_newc (ovm_t ovm, ovm_inst_t * dst, unsigned size)
{
  _ovm_inst_alloc (ovm, ovm_cl_array, dst);

  __ovm_array_init (ovm, *dst, size);
}

static void
_ovm_array_init (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst,
		 unsigned argc, ovm_inst_t * argv)
{
  ovm_inst_t arg;
  ovm_class_t arg_cl;
  unsigned size;

  OVM_ASSERT (argc >= 1);
  arg = argv[0];

  arg_cl = ovm_inst_of (arg);

  if (arg_cl == ovm_cl_integer)
    {
      size = INTVAL (arg);
    }
  else if (arg_cl == ovm_cl_list)
    {
      size = __ovm_list_len (arg);
    }
  else if (ovm_is_subclass_of (arg_cl, ovm_cl_set))
    {
      size = SETVAL (arg)->cnt;
    }
  else
    {
      OVM_ASSERT (0);
    }

  __ovm_array_init (ovm, inst, size);

  if (arg_cl == ovm_cl_list)
    {
      ovm_inst_t *p;

      for (p = ARRAYVAL (inst)->data; arg; arg = CDR (arg), ++p)
	{
	  _ovm_assign (ovm, p, CAR (arg));
	}
    }
  else if (ovm_is_subclass_of (arg_cl, ovm_cl_set))
    {
      ovm_inst_t *p, q, *r;
      unsigned n;

      for (r = ARRAYVAL (inst)->data, p = SETVAL (arg)->base->data, n =
	   SETVAL (arg)->base->size; n; --n, ++p)
	{
	  for (q = *p; q; q = CDR (q), ++r)
	    {
	      _ovm_assign (ovm, r, CAR (q));
	    }
	}
    }

  --argc;
  ++argv;

  _ovm_init_parent (ovm, cl, inst, argc, argv);
}

static void
_ovm_array_walk (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst,
		 void (*func) (struct ovm *, ovm_inst_t))
{
  ovm_inst_t *p;
  unsigned n;

  for (p = ARRAYVAL (inst)->data, n = ARRAYVAL (inst)->size; n; --n, ++p)
    {
      (*func) (ovm, *p);
    }

  _ovm_walk_parent (ovm, cl, inst, func);
}

static void
_ovm_array_free (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst)
{
  _ovm_free (ovm, (void **) &ARRAYVAL (inst)->data,
	     __ovm_array_size_bytes (ARRAYVAL (inst)->size));

  _ovm_free_parent (ovm, cl, inst);
}

struct ovm_class ovm_cl_array[1] = { {
				      .name = "Array",
				      .parent = ovm_cl_object,
				      .new = _ovm_inst_new2,
				      .init = _ovm_array_init,
				      .walk = _ovm_array_walk,
				      .free = _ovm_array_free,
				      .inst_method_func_tbl = {
							       }
				      }
};

/***************************************************************************/

static ovm_inst_t *
__ovm_set_find (ovm_t ovm, ovm_inst_t set, ovm_inst_t val, ovm_inst_t ** bb)
{
  ovm_inst_t *result = 0, *b, *p, q;

  OVM_FRAME_DECL (fr, work);

  OVM_FRAME_ENTER (ovm, fr);

  OVM_INST_METHOD_CALL (ovm, fr->work, val, OVM_INST_METHOD_SEL_HASH);

  b =
    &SETVAL (set)->
    base->data[INTVAL (fr->work) & (SETVAL (set)->base->size - 1)];

  for (p = b; q = *p; p = &CDR (q))
    {
      OVM_INST_METHOD_CALL (ovm, fr->work, val,
			    OVM_INST_METHOD_SEL_EQUAL, CAR (q));

      if (BOOLVAL (fr->work))
	{
	  result = p;

	  break;
	}
    }

  if (bb)
    *bb = b;

  OVM_FRAME_LEAVE (ovm);

  return (result);
}

static void
__ovm_set_put (ovm_t ovm, ovm_inst_t set, ovm_inst_t val)
{
  ovm_inst_t *p, *b;

  p = __ovm_set_find (ovm, set, val, &b);

  if (p == 0)
    {
      OVM_FRAME_DECL (fr, work);

      OVM_FRAME_ENTER (ovm, fr);

      _ovm_inst_alloc (ovm, ovm_cl_list, &fr->work);

      OVM_ASSIGN (ovm, CAR (fr->work), val);
      OVM_ASSIGN (ovm, CDR (fr->work), *b);
      _ovm_assign (ovm, b, fr->work);

      OVM_FRAME_LEAVE (ovm);
    }
}

static void
_ovm_set_copy (ovm_t ovm, ovm_inst_t * dst, ovm_inst_t src)
{
  ovm_inst_t *p, *q, r, *s, t;
  unsigned n;

  OVM_FRAME_DECL (fr, work);

  OVM_FRAME_ENTER (ovm, fr);

  _ovm_inst_alloc (ovm, ovm_inst_of (src), &fr->work);

  for (q = SETVAL (fr->work)->base->data, p =
       SETVAL (src)->base->data, n = SETVAL (src)->base->size; n;
       --n, ++p, ++q)
    {
      for (s = q, r = *p; r; r = CDR (r))
	{
	  _ovm_inst_alloc (ovm, ovm_cl_list, s);
	  OVM_ASSIGN (ovm, CAR (*s), CAR (r));

	  s = &CDR (*s);
	}
    }

  SETVAL (fr->work)->cnt = SETVAL (src)->cnt;

  _ovm_assign (ovm, dst, fr->work);

  OVM_FRAME_LEAVE (ovm);
}

static unsigned
round_up_to_power_of_2 (unsigned u)
{
  unsigned v;

  v = u & (u - 1);
  if (v == 0)
    return (u);

  for (;;)
    {
      u = v;
      v = u & (u - 1);
      if (v == 0)
	return (u << 1);
    }
}

static void
_ovm_set_init (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst,
	       unsigned argc, ovm_inst_t * argv)
{
  ovm_intval_t size = OVM_SET_SIZE_DFLT;
  ovm_inst_t arg;
  ovm_class_t arg_cl = 0;

  if (argc >= 1)
    {
      arg = argv[0];
      arg_cl = ovm_inst_of (arg);

      if (arg_cl == ovm_cl_integer)
	{
	  size = INTVAL (arg);

	  OVM_ASSERT (size >= 0);
	}

      --argc;
      ++argv;
    }

  __ovm_array_init (ovm, inst, round_up_to_power_of_2 (size));

  if (arg_cl == ovm_cl_list)
    {
      for (; arg; arg = CDR (arg))
	{
	  __ovm_set_put (ovm, inst, CAR (arg));
	}
    }
  else if (arg_cl == ovm_cl_array)
    {
      ovm_inst_t *p;
      unsigned n;

      for (p = ARRAYVAL (arg)->data, n = ARRAYVAL (arg)->size; n; --n, ++p)
	{
	  __ovm_set_put (ovm, inst, *p);
	}
    }
  else if (cl == ovm_cl_dict)
    {
      ovm_inst_t *p, q;
      unsigned n;

      for (p = DICTVAL (arg)->base->base->data, n =
	   DICTVAL (arg)->base->base->size; n; --n, ++p)
	{
	  for (q = *p; q; q = CDR (q))
	    {
	      __ovm_set_put (ovm, inst, CAR (q));
	    }
	}
    }
  else
    {
      OVM_ASSERT (0);
    }

  _ovm_object_init (ovm, cl, inst, argc, argv);
}

static void
_ovm_set_in (ovm_t ovm, ovm_inst_t * dst, unsigned argc, ovm_inst_t * argv)
{
  OVM_ASSERT (argc == 1);
  OVM_ASSERT (ovm_inst_of (argv[0]) == ovm_cl_set);

  __ovm_bool_newc (ovm, dst,
		   __ovm_set_find (ovm, argv[0], argv[1], 0) ? 1 : 0);
}

static void
_ovm_set_put (ovm_t ovm, ovm_inst_t * dst, unsigned argc, ovm_inst_t * argv)
{
  OVM_ASSERT (argc == 1);
  OVM_ASSERT (ovm_inst_of (argv[0]) == ovm_cl_set);

  __ovm_set_put (ovm, argv[0], argv[1]);
}

struct ovm_class ovm_cl_set[1] = { {
				    .name = "Set",
				    .parent = ovm_cl_array,
				    .new = _ovm_inst_new2,
				    .init = _ovm_set_init,
				    .walk = _ovm_walk_parent,
				    .free = _ovm_free_parent,
				    .inst_method_func_tbl = {
							     [OVM_INST_METHOD_SEL_IN] = _ovm_set_in,
							     [OVM_INST_METHOD_SEL_PUT] = _ovm_set_put}
				    }
};

/***************************************************************************/

static ovm_inst_t *
__ovm_dict_find (ovm_t ovm, ovm_inst_t dict, ovm_inst_t key, ovm_inst_t ** bb)
{
  ovm_inst_t *result = 0, *b, *p, q;

  OVM_FRAME_DECL (fr, work);

  OVM_FRAME_ENTER (ovm, fr);

  OVM_INST_METHOD_CALL (ovm, fr->work, key, OVM_INST_METHOD_SEL_HASH);

  b =
    &DICTVAL (dict)->base->base->data[INTVAL (fr->work)
				      & (DICTVAL (dict)->base->base->size -
					 1)];

  for (p = b; q = *p; p = &CDR (q))
    {
      OVM_INST_METHOD_CALL (ovm, fr->work, key,
			    OVM_INST_METHOD_SEL_EQUAL, CAR (CAR (q)));

      if (BOOLVAL (fr->work))
	{
	  result = p;

	  break;
	}
    }

  if (bb)
    *bb = b;

  OVM_FRAME_LEAVE (ovm);

  return (result);
}

static void
__ovm_dict_at_put (ovm_t ovm, ovm_inst_t dict, ovm_inst_t key, ovm_inst_t val)
{
  ovm_inst_t *p, *b;

  p = __ovm_dict_find (ovm, dict, key, &b);

  if (p)
    {
      OVM_ASSIGN (ovm, CDR (CAR (*p)), val);
    }
  else
    {
      OVM_FRAME_DECL (fr, work);

      OVM_FRAME_ENTER (ovm, fr);

      _ovm_inst_alloc (ovm, ovm_cl_list, &fr->work);
      _ovm_inst_alloc (ovm, ovm_cl_pair, &CAR (fr->work));
      OVM_ASSIGN (ovm, CAR (CAR (fr->work)), key);
      OVM_ASSIGN (ovm, CDR (CAR (fr->work)), val);

      OVM_ASSIGN (ovm, CDR (fr->work), *b);
      _ovm_assign (ovm, b, fr->work);

      OVM_FRAME_LEAVE (ovm);
    }
}

static void
__ovm_dict_put (ovm_t ovm, ovm_inst_t dict, ovm_inst_t x)
{
  ovm_inst_t key, val;

  if (ovm_inst_of (x) == ovm_cl_pair)
    {
      key = CAR (x);
      val = CDR (x);
    }
  else
    {
      key = x;
      val = OVM_NIL;
    }

  __ovm_dict_at_put (ovm, dict, key, val);
}

static void
_ovm_dict_copy (ovm_t ovm, ovm_inst_t * dst, ovm_inst_t src)
{
  ovm_inst_t *p, *q, r, *s, t, u, v;
  unsigned n;

  OVM_FRAME_DECL (fr, work);

  OVM_FRAME_ENTER (ovm, fr);

  _ovm_inst_alloc (ovm, ovm_cl_dict, &fr->work);

  for (q = DICTVAL (fr->work)->base->base->data, p =
       DICTVAL (src)->base->base->data, n = DICTVAL (src)->base->base->size;
       n; --n, ++p, ++q)
    {
      for (s = q, r = *p; r; r = CDR (r))
	{
	  _ovm_inst_alloc (ovm, ovm_cl_list, s);
	  _ovm_inst_alloc (ovm, ovm_cl_pair, &CAR (*s));

	  OVM_ASSIGN (ovm, CAR (CAR (*s)), CAR (CAR (r)));
	  OVM_ASSIGN (ovm, CDR (CAR (*s)), CDR (CAR (r)));

	  s = &CDR (*s);
	}
    }

  _ovm_assign (ovm, dst, fr->work);

  OVM_FRAME_LEAVE (ovm);
}

static void
_ovm_dict_init (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst,
		unsigned argc, ovm_inst_t * argv)
{
  unsigned size = OVM_SET_SIZE_DFLT;
  ovm_inst_t arg;
  ovm_class_t arg_cl = 0;

  if (argc >= 1)
    {
      arg = argv[0];
      arg_cl = ovm_inst_of (arg);

      if (arg_cl == ovm_cl_integer)
	{
	  size = INTVAL (arg);

	  OVM_ASSERT (size >= 0);
	}

      --argc;
      ++argv;
    }

  __ovm_array_init (ovm, inst, round_up_to_power_of_2 (size));

  if (arg_cl == ovm_cl_list)
    {
      for (; arg; arg = CDR (arg))
	{
	  __ovm_dict_put (ovm, inst, CAR (arg));
	}
    }
  else if (arg_cl == ovm_cl_array)
    {
      ovm_inst_t *p;
      unsigned n;

      for (p = ARRAYVAL (arg)->data, n = ARRAYVAL (arg)->size; n; --n, ++p)
	{
	  __ovm_dict_put (ovm, inst, *p);
	}
    }
  else if (arg_cl == ovm_cl_set)
    {
      ovm_inst_t *p, q;
      unsigned n;

      for (p = SETVAL (arg)->base->data, n = SETVAL (arg)->base->size; n;
	   --n, ++p)
	{
	  for (q = *p; q; q = CDR (q))
	    {
	      __ovm_dict_put (ovm, inst, CAR (q));
	    }
	}
    }

  _ovm_object_init (ovm, cl, inst, argc, argv);
}

struct ovm_class ovm_cl_dict[1] = { {
				     .name = "Dictionary",
				     .parent = ovm_cl_set,
				     .new = _ovm_inst_new2,
				     .init = _ovm_dict_init,
				     .walk = _ovm_walk_parent,
				     .free = _ovm_free_parent,
				     .inst_method_func_tbl = {
							      }
				     }
};

/***************************************************************************/

#define PRINT_STAT(x)  printf("%s\t= %llu\n", #x, ovm->stats-> x)

void
ovm_stats_print (ovm_t ovm)
{
  printf ("\n\novm statistics:\n");
  PRINT_STAT (insts_in_use);
  PRINT_STAT (insts_max);
  PRINT_STAT (pages_in_use);
  PRINT_STAT (pages_max);
  PRINT_STAT (mem_in_use);
  PRINT_STAT (mem_max);
}
