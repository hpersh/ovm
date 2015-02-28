#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>

#include "ovm.h"

#ifndef NDEBUG
#define OVM_DEBUG_ASSERT(_x)  assert(_x)
#else
#define OVM_DEBUG_ASSERT(_x)
#endif
#define OVM_ASSERT(_x)  assert(_x)


static void __ovm_inst_free (ovm_t ovm, ovm_inst_t inst);
static ovm_inst_t __ovm_inst_retain (ovm_t ovm, ovm_inst_t inst);
static void __ovm_inst_release (ovm_t ovm, ovm_inst_t inst);

static void _ovm_inst_alloc (ovm_t ovm, ovm_class_t cl, ovm_inst_t * dst);


enum {
  CRC32_POLYNOMIAL = 0x04c11db7
};

static unsigned
_crc32(unsigned *r, void *p, unsigned n)
{
  static unsigned tbl[256];
  static unsigned char tbl_genf = 1;

  unsigned char *q;
  unsigned i, j, a;

  if (tbl_genf) {
    for (i = 0; i < 256; ++i)
      {
	a = i << 24;
	
	for (j = 0; j < 8; ++j)
	  {
	    a = (a << 1) ^ ((unsigned) -(int)(a >> 31) & CRC32_POLYNOMIAL);
	  }
	tbl[i] = a;
      }
    
    tbl_genf = 0;
  }
  
  for (a = *r, q = (unsigned char *) p; n; --n, ++q)
    {
      a = (a << 8) ^ tbl[((a >> 24) ^ *q) & 0xff];
    }
  
  *r = a;
}

static inline void
_crc32_init(unsigned *r)
{
  *r = (unsigned) -1;
}

static inline unsigned
_crc32_get(unsigned *r)
{
  return (~*r);
}


static inline void
_sv_init(struct ovm_strval *sv, unsigned n, char *p)
{
  sv->size = n;
  sv->data = p;
}

static inline char *
_sv_data(struct ovm_strval *sv)
{
  return (sv->data);
}

static inline unsigned
_sv_size(struct ovm_strval *sv)
{
  return (sv->size);
}

static inline unsigned
_sv_eof(struct ovm_strval *sv)
{
  return (sv->size == 0);
}

static inline void
_sv_adv(struct ovm_strval *sv, unsigned n)
{
  OVM_ASSERT(sv->size >= n);

  sv->data += n;
  sv->size -= n;
}

static inline void
_sv_trim(struct ovm_strval *sv, unsigned n)
{
  OVM_ASSERT(sv->size >= n);

  sv->size -= n;
}

static char
_sv_getc(struct ovm_strval *sv)
{
  char result;

  if (_sv_eof(sv))  return (-1);

  result = *sv->data;

  ++sv->data;  --sv->size;

  return (result);
}

static inline void
_sv_ungetc(struct ovm_strval *sv)
{
  --sv->data;  ++sv->size;
}

static inline char
_sv_peek(struct ovm_strval *sv, unsigned ofs)
{
  char result;

  return (ofs >= sv->size ? -1 : sv->data[ofs]);
}

static inline unsigned
_sv_strcmp(struct ovm_strval *sv, unsigned ofs, unsigned n, char *s)
{
  return (sv->size >= (ofs + n) && strncmp(sv->data + ofs, s, n) == 0);
}

static inline unsigned
_sv_strcmp2(struct ovm_strval *sv, unsigned ofs, struct ovm_strval *sv2)
{
  return (_sv_strcmp(sv, ofs, sv2->size, sv2->data));
}

static
_sv_space_skip(struct ovm_strval *sv)
{
  for ( ; sv->size; --sv->size, ++sv->data) {
    if (!isspace(*sv->data))  break;
  }
}




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
  fr->end = fr->start + fr->size;
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

#ifndef NOSTATS
  ADD_MAX (ovm->stats->mem_in_use, ovm->stats->mem_max, size);
#endif

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

#ifndef NOSTATS
      ovm->stats->mem_in_use -= size;
#endif

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

#ifndef NOSTATS
      ADD_MAX (ovm->stats->pages_in_use, ovm->stats->pages_max, 1);
#endif

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

#ifndef NOSTATS
  ADD_MAX (ovm->stats->insts_in_use, ovm->stats->insts_max, 1);
#endif

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

#ifndef NOSTATS
  --ovm->stats->insts_in_use;
#endif

  if (--p->in_use_cnt == 0)
    {
      for (q = (ovm_inst_t) (p + 1), n = ovm->insts_per_page; n; --n, ++q)
	{
	  list_erase (q->list_node);
	}

      list_erase (p->list_node);

      _ovm_free (ovm, (void **) &p, ovm->inst_page_size);

#ifndef NOSTATS
      --ovm->stats->pages_in_use;
#endif
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

static inline ovm_inst_t
_ovm_inst_chk (ovm_inst_t inst)
{
  OVM_ASSERT (inst == 0 || inst->magic == _OVM_INST_MAGIC);

  return (inst);
}

static void
__ovm_inst_new_chk(ovm_t ovm, ovm_inst_t * dst, unsigned argc, va_list ap)
{
  _ovm_dst_chk(ovm, dst);

  for ( ; argc; --argc) {
    _ovm_inst_chk(va_arg(ap, ovm_inst_t));
  }
}

static void
__ovm_method_call_chk(ovm_t ovm, ovm_inst_t * dst, ovm_inst_t rcvr,
		     unsigned argc, va_list ap)
{
  _ovm_dst_chk(ovm, dst);

  _ovm_inst_chk(rcvr);

  for ( ; argc; --argc) {
    _ovm_inst_chk(va_arg(ap, ovm_inst_t));
  }
}

#endif	/* !defined(NDEBUG) */

static void
__ovm_inst_new_run (ovm_t ovm, ovm_inst_t * dst, ovm_class_t cl, unsigned argc, va_list ap)
{
  ovm_inst_t argv[argc], *p;
  unsigned n;

  for (p = argv, n = argc; n; --n, ++p)
    {
      *p = va_arg (ap, ovm_inst_t);
    }

  (*cl->new) (ovm, cl, dst, argc, argv);
}

static void
__ovm_inst_new (ovm_t ovm, ovm_inst_t * dst, ovm_class_t cl, unsigned argc,
	       ...)
{
  va_list ap;

  va_start (ap, argc);

  __ovm_inst_new_run(ovm, dst, cl, argc, ap);

  va_end (ap);
}

void
_ovm_inst_new (ovm_t ovm, ovm_inst_t * dst, ovm_class_t cl, unsigned argc,
	       ...)
{
  va_list ap;

#ifndef NDEBUG
  va_start (ap, argc);

  __ovm_inst_new_chk(ovm, dst, argc, ap);

  va_end (ap);
#endif

  va_start (ap, argc);

  __ovm_inst_new_run(ovm, dst, cl, argc, ap);

  va_end (ap);
}

static void
__ovm_method_call_run (ovm_t ovm, ovm_inst_t * dst, ovm_inst_t rcvr,
		      ovm_class_t cl, unsigned sel, unsigned argc, va_list ap)
{
  unsigned n;
  ovm_inst_t argv[1 + argc], *p;
  void (*f) (struct ovm *, ovm_inst_t *, unsigned, ovm_inst_t *);

  argv[0] = rcvr;

  for (p = &argv[1], n = argc; n; --n, ++p)
    {
      *p = va_arg (ap, ovm_inst_t);
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

static void
__ovm_inst_method_call (ovm_t ovm, ovm_inst_t * dst, ovm_inst_t rcvr,
		       unsigned sel, unsigned argc, ...)
{
  va_list ap;

  va_start (ap, argc);

  __ovm_method_call_run (ovm, dst, rcvr, ovm_inst_of (rcvr), sel, argc, ap);

  va_end (ap);
}


void
_ovm_inst_method_call_cl (ovm_t ovm, ovm_inst_t * dst, ovm_inst_t rcvr,
			  ovm_class_t cl, unsigned sel, unsigned argc, ...)
{
  va_list ap;

#ifndef NDEBUG
  va_start (ap, argc);

  __ovm_method_call_chk (ovm, dst, rcvr, argc, ap);

  va_end (ap);
#endif

  va_start (ap, argc);

  __ovm_method_call_run (ovm, dst, rcvr, cl, sel, argc, ap);

  va_end (ap);
}

void
_ovm_inst_method_call (ovm_t ovm, ovm_inst_t * dst, ovm_inst_t rcvr,
		       unsigned sel, unsigned argc, ...)
{
  va_list ap;

#ifndef NDEBUG
  va_start (ap, argc);

  __ovm_method_call_chk (ovm, dst, rcvr, argc, ap);

  va_end (ap);
#endif

  va_start (ap, argc);

  __ovm_method_call_run (ovm, dst, rcvr, ovm_inst_of (rcvr), sel, argc, ap);

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

static inline unsigned
bmap_unit_idx (unsigned bit)
{
  return (bit >> OVM_BMVAL_UNIT_BITS_LOG2);
}

static inline unsigned
bmap_unit_sh (unsigned bit)
{
  return (bit & (OVM_BMVAL_UNIT_BITS - 1));
}

static inline unsigned
bmap_bits_to_units (unsigned bits)
{
  OVM_ASSERT (bits > 0);

  return (bmap_unit_idx (bits - 1) + 1);
}

static inline unsigned
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
#ifndef NDEBUG
  _ovm_dst_chk (ovm, dst);
#endif

  __ovm_bool_newc (ovm, dst, val);
}

static void
_xml_parse_bool2 (ovm_t ovm, ovm_inst_t inst, struct ovm_strval *pb)
{
  char c;

  _sv_space_skip(pb);
  c = _sv_getc(pb) | 0x20;
  OVM_ASSERT (c == 't' || c == 'f');

  BOOLVAL (inst) = (c == 't');

  _sv_space_skip(pb);
  OVM_ASSERT(_sv_strcmp(pb, 0, 10, "</Boolean>"));
  _sv_adv(pb, 10);
}

static void
_xml_parse_bool (ovm_t ovm, ovm_inst_t inst, struct ovm_strval *pb)
{
  _sv_space_skip(pb);
  OVM_ASSERT(_sv_strcmp(pb, 0, 9, "<Boolean>"));
  _sv_adv(pb, 9);
  _xml_parse_bool2 (ovm, inst, pb);
}

static void
_xml_parse_str(ovm_t ovm, ovm_inst_t inst, ovm_inst_t s, void (*func)(ovm_t, ovm_inst_t, struct ovm_strval *))
{
  struct ovm_strval pb[1];
  
  *pb = *STRVAL(s);
  _sv_trim(pb, 1);
  (*func)(ovm, inst, pb);
  _sv_space_skip(pb);
  OVM_ASSERT(_sv_eof(pb));
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
	  _xml_parse_str(ovm, inst, arg, _xml_parse_bool);
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
#ifndef NDEBUG
  _ovm_dst_chk (ovm, dst);
#endif

  __ovm_integer_newc (ovm, dst, val);
}

static void
_xml_parse_int2 (ovm_t ovm, ovm_inst_t inst, struct ovm_strval *pb)
{
  unsigned negf = 0, ndigs;
  ovm_intval_t val = 0;
  char c;

  _sv_space_skip(pb);
  if (_sv_getc(pb) == '-') {
    negf = 1;
  } else {
    _sv_ungetc(pb);
  }

  for (ndigs = 0;;) {
    c = _sv_getc(pb);
    if (c == '<') {
      _sv_ungetc(pb);
      break;
    }
    if (isspace(c)) {
      _sv_space_skip(pb);
      break;
    }

    OVM_ASSERT (c >= '0' && c <= '9');
    val = 10 * val + (c - '0');

    ++ndigs;
  }

  OVM_ASSERT(_sv_strcmp(pb, 0, 10, "</Integer>"));
  _sv_adv(pb, 10);

  OVM_ASSERT(ndigs > 0);

  if (negf)
    val = -val;
  INTVAL (inst) = val;
}

static void
_xml_parse_int (ovm_t ovm, ovm_inst_t inst, struct ovm_strval *pb)
{
  _sv_space_skip(pb);
  OVM_ASSERT(_sv_strcmp(pb, 0, 9, "<Integer>"));
  _sv_adv(pb, 9);
  _xml_parse_int2 (ovm, inst, pb);
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
	  OVM_ASSERT (sscanf (_sv_data(STRVAL (arg)), "%lld", &INTVAL (inst)) ==
		      1);
	}
      else if (arg_cl == ovm_cl_xml)
	{
	  _xml_parse_str(ovm, inst, arg, _xml_parse_int);
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

static void
_ovm_integer_hash (ovm_t ovm, ovm_inst_t * dst, unsigned argc,
		  ovm_inst_t * argv)
  {
    unsigned h[1];

  OVM_ASSERT (argc == 0);
  OVM_ASSERT (ovm_is_kind_of (argv[0], ovm_cl_integer));

  _crc32_init(h);
  _crc32(h, &INTVAL(argv[0]), sizeof(INTVAL(argv[0])));
  
  __ovm_integer_newc(ovm, dst, _crc32_get(h));
 }
 
 struct ovm_class ovm_cl_integer[1] = { {
					.name = "Integer",
					.parent = ovm_cl_num,
					.new = _ovm_inst_new1,
					.init = _ovm_integer_init,
					.walk = _ovm_walk_parent,
					.free = _ovm_free_parent,
					.inst_method_func_tbl = {
      [OVM_INST_METHOD_SEL_ADD]  = _ovm_integer_add,
      [OVM_INST_METHOD_SEL_HASH] = _ovm_integer_hash
    }
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
#ifndef NDEBUG
  _ovm_dst_chk (ovm, dst);
#endif

  __ovm_float_newc (ovm, dst, val);
}

static void
_xml_parse_float2 (ovm_t ovm, ovm_inst_t inst, struct ovm_strval *pb)
{
  unsigned mnegf = 0, dcnt = 0, dpf = 0, ef = 0, enegf = 0;
  unsigned n;
  struct ovm_strval pbtok[1];
  char c;

  _sv_space_skip(pb);
  *pbtok = *pb;

  for (;;) {
    c = _sv_getc(pb);
    
    if (c == '<') {
      _sv_ungetc(pb);
      _sv_trim(pbtok, _sv_size(pb));
      break;
    }
    if (isspace(c)) {
      _sv_trim(pbtok, _sv_size(pb) + 1);
      _sv_space_skip(pb);
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
    }

  OVM_ASSERT (dcnt > 0);
  OVM_ASSERT (_sv_strcmp(pb, 0, 8, "</Float>"));
  _sv_adv(pb, 8);

  {
    unsigned n = _sv_size(pbtok);
    char buf[n + 1];

    memcpy(buf, _sv_data(pbtok), n);
    buf[n] = 0;

    OVM_ASSERT (sscanf (buf, "%Lg", &FLOATVAL (inst)) == 1);
  }
}

static void
_xml_parse_float (ovm_t ovm, ovm_inst_t inst, struct ovm_strval *pb)
{
  _sv_space_skip(pb);
  OVM_ASSERT (_sv_strcmp (pb, 0, 7, "<Float>"));
  _sv_adv(pb, 7);
  _xml_parse_float2 (ovm, inst, pb);
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
	  OVM_ASSERT (sscanf (_sv_data(STRVAL (arg)), "%Lg", &FLOATVAL (inst)) ==
		      1);
	}
      else if (arg_cl == ovm_cl_xml)
	{
	  _xml_parse_str(ovm, inst, arg, _xml_parse_float);
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
#ifndef NDEBUG
  _ovm_dst_chk (ovm, dst);
#endif

  __ovm_string_newc (ovm, dst, s);
}

static void
_xml_parse_string2 (ovm_t ovm, ovm_inst_t inst, struct ovm_strval *pb)
{
  struct ovm_strval pb2[1];
  unsigned k;

  *pb2 = *pb;

  for (;;) {
    if (_sv_getc(pb) == '<') {
      _sv_ungetc(pb);
      break;
    }
  }

  _sv_trim(pb2, _sv_size(pb));

  OVM_ASSERT(_sv_strcmp(pb, 0, 9, "</String>"));
  _sv_adv(pb, 9);

  {
    char buf[_sv_size(pb2) + 1], *r, c;

    for (r = buf;;) {
      {
	c = _sv_getc(pb2);
	if (c == -1)  break;
	if (c == '&')
	  {
	    if (_sv_strcmp (pb2, 0, 6, "&quot;"))
	      {
		c = '"';
		_sv_adv(pb2, 6);
	      }
	    else if (_sv_strcmp (pb2, 0, 6, "&apos;"))
	      {
		c = '\'';
		_sv_adv(pb2, 6);
	      }
	    else if (_sv_strcmp (pb2, 0, 5, "&amp;"))
	      {
		c = '&';
		_sv_adv(pb2, 5);
	      }
	    else if (_sv_strcmp (pb2, 0, 4, "&lt;"))
	      {
		c = '<';
		_sv_adv(pb2, 4);
	      }
	    else if (_sv_strcmp (pb2, 0, 4, "&gt;"))
	      {
		c = '>';
		_sv_adv(pb2, 4);
	      }
	    else
	      {
		OVM_ASSERT (0);
	      }
	  }

	*r = c;
	++r;
      }
    }

    k = r - buf;
    STRVAL(inst)->size = k + 1;
    _ovm_malloc(ovm, _sv_size(STRVAL(inst)), (void **) &STRVAL(inst)->data);
    memcpy(_sv_data(STRVAL(inst)), buf, k);
    _sv_data(STRVAL(inst))[k] = 0;
  }
}

static void
_xml_parse_string (ovm_t ovm, ovm_inst_t inst, struct ovm_strval *pb)
{
  _sv_space_skip (pb);
  OVM_ASSERT (_sv_strcmp (pb, 0, 8, "<String>"));
  _sv_adv(pb, 8);
  _xml_parse_string2 (ovm, inst, pb);
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
	  _xml_parse_str(ovm, inst, arg, _xml_parse_string);
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
	  __ovm_inst_new (ovm, &fr->work[1], ovm_cl_string, 1, CAR (arg));
	  __ovm_string_newc (ovm, &fr->work[2], ", ");
	  __ovm_inst_new (ovm, &fr->work[3], ovm_cl_string, 1, CDR (arg));
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
		__ovm_inst_new (ovm, q, ovm_cl_string, 1, CAR (arg));
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
		__ovm_inst_new (ovm, q, ovm_cl_string, 1, *p);
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
		    __ovm_inst_new (ovm, q, ovm_cl_string, 1, CAR (r));
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

		    __ovm_inst_new (ovm, q, ovm_cl_string, 1, CAR (CAR (r)));
		    ++q;
		    _ovm_assign (ovm, q, fr->delim[1]);
		    ++q;
		    __ovm_inst_new (ovm, q, ovm_cl_string, 1, CDR (CAR (r)));
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
_ovm_string_hash (ovm_t ovm, ovm_inst_t * dst, unsigned argc,
		  ovm_inst_t * argv)
{
  unsigned h[1];

  OVM_ASSERT (argc == 0);
  OVM_ASSERT (ovm_is_kind_of (argv[0], ovm_cl_string));
 
  _crc32_init(h);
  _crc32(h, STRVAL(argv[0])->data, STRVAL(argv[0])->size - 1);

  __ovm_integer_newc(ovm, dst, _crc32_get(h));
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
      [OVM_INST_METHOD_SEL_AT_LEN] = _ovm_string_at_len, 
      [OVM_INST_METHOD_SEL_HASH]   = _ovm_string_hash }
				       }
};

/***************************************************************************/

void
_ovm_xml_newc (ovm_t ovm, ovm_inst_t * dst, char *s)
{
#ifndef NDEBUG
  _ovm_dst_chk (ovm, dst);
#endif

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

	  _sv_init(STRVAL(inst), k, buf);
	}
      else if (arg_cl == ovm_cl_bmap)
	{
	  unsigned n = BMVAL (arg)->size, nn;
	  unsigned k = 8 + n + 9 + 1;
	  ovm_bmval_unit_t *p, u;
	  char *buf = 0, *q;

	  _ovm_malloc (ovm, k, (void **) &buf);

	  q = buf;
	  strcpy (q, "<Bitmap>");
	  q += 8;

	  for (p = BMVAL (arg)->data; n; ++p)
	    {
	      for (u = *p, nn = OVM_BMVAL_UNIT_BITS; nn && n; --nn, --n, ++q, u >>= 1)
		{
		  *q = (u & 1) ? '1' : '0';
		}
	    }

	  strcpy (q, "</Bitmap>");

	  _sv_init(STRVAL(inst), k, buf);
	}
      else if (arg_cl == ovm_cl_pair)
	{
	  OVM_FRAME_DECL (fr, work[4]);

	  OVM_FRAME_ENTER (ovm, fr);

	  __ovm_string_newc (ovm, &fr->work[0], "<Pair>");
	  __ovm_inst_new (ovm, &fr->work[1], ovm_cl_xml, 1, CAR (arg));
	  __ovm_inst_new (ovm, &fr->work[2], ovm_cl_xml, 1, CDR (arg));
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
		__ovm_inst_new (ovm, q, ovm_cl_xml, 1, CAR (arg));
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
		__ovm_inst_new (ovm, q, ovm_cl_xml, 1, *p);
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
		    __ovm_inst_new (ovm, q, ovm_cl_string, 1, CAR (r));
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
		    __ovm_inst_new (ovm, q, ovm_cl_string, 1, CAR (r));
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

static void __ovm_array_init (ovm_t ovm, ovm_inst_t inst, unsigned size);
static void _xml_parse_bmap2(ovm_t ovm, ovm_inst_t inst, struct ovm_strval *pb);
static void _xml_parse_pair2 (ovm_t ovm, ovm_inst_t inst, struct ovm_strval *pb);
static void _xml_parse_list2 (ovm_t ovm, ovm_inst_t *dst, struct ovm_strval *pb);
static void _xml_parse_array2 (ovm_t ovm, ovm_inst_t dst, struct ovm_strval *pb);
static void _xml_parse_set2 (ovm_t ovm, ovm_inst_t dst, struct ovm_strval *pb);
static void _xml_parse_dict2 (ovm_t ovm, ovm_inst_t dst, struct ovm_strval *pb);

static void
__xml_parse(ovm_t ovm, ovm_inst_t *dst, struct ovm_strval *pb)
{
  OVM_FRAME_DECL(fr, work);

  _sv_space_skip(pb);

  OVM_FRAME_ENTER(ovm, fr);

  if (_sv_strcmp (pb, 0, 9, "<Boolean>")) {
    _ovm_inst_alloc(ovm, ovm_cl_bool, &fr->work);
    _sv_adv(pb, 9);
    _xml_parse_bool2 (ovm, fr->work, pb);
  } else if (_sv_strcmp(pb, 0, 9, "<Integer>")) {
    _ovm_inst_alloc(ovm, ovm_cl_integer, &fr->work);
    _sv_adv(pb, 9);
    _xml_parse_int2 (ovm, fr->work, pb);
  } else if (_sv_strcmp(pb, 0, 7, "<Float>")) {
    _ovm_inst_alloc(ovm, ovm_cl_float, &fr->work);
    _sv_adv(pb, 7);
    _xml_parse_float2 (ovm, fr->work, pb);
  } else if (_sv_strcmp(pb, 0, 8, "<String>")) {
    _ovm_inst_alloc(ovm, ovm_cl_string, &fr->work);
    _sv_adv(pb, 8);
    _xml_parse_string2 (ovm, fr->work, pb);
  } else if (_sv_strcmp(pb, 0, 8, "<Bitmap>")) {
    _ovm_inst_alloc(ovm, ovm_cl_bmap, &fr->work);
    _sv_adv(pb, 8);
    _xml_parse_bmap2 (ovm, fr->work, pb);
  } else if (_sv_strcmp(pb, 0, 6, "<Pair>")) {
    _sv_adv(pb, 6);
    _ovm_inst_alloc(ovm, ovm_cl_pair, &fr->work);
    _xml_parse_pair2 (ovm, fr->work, pb);
  } else if (_sv_strcmp(pb, 0, 6, "<List>")) {
    _sv_adv(pb, 6);
    _xml_parse_list2 (ovm, &fr->work, pb);
  } else if (_sv_strcmp(pb, 0, 7, "<Array>")) {
    _sv_adv(pb, 7);
    _ovm_inst_alloc(ovm, ovm_cl_array, &fr->work);
    _xml_parse_array2 (ovm, fr->work, pb);
  } else if (_sv_strcmp(pb, 0, 5, "<Set>")) {
    _sv_adv(pb, 5);
    _ovm_inst_alloc(ovm, ovm_cl_set, &fr->work);
    __ovm_array_init (ovm, fr->work, OVM_SET_SIZE_DFLT);
    _xml_parse_set2 (ovm, fr->work, pb);
  } else if (_sv_strcmp(pb, 0, 12, "<Dictionary>")) {
    _sv_adv(pb, 12);
    _ovm_inst_alloc(ovm, ovm_cl_dict, &fr->work);
    __ovm_array_init (ovm, fr->work, OVM_DICT_SIZE_DFLT);
    _xml_parse_dict2 (ovm, fr->work, pb);
  } else {
    OVM_ASSERT(0);
  }
  
  _ovm_assign(ovm, dst, fr->work);
  
  OVM_FRAME_LEAVE(ovm);
}

static void
_ovm_xml_parse(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  struct ovm_strval pb[1];

  OVM_ASSERT(argc == 0);
  OVM_ASSERT(ovm_is_kind_of(argv[0], ovm_cl_xml));

  *pb = *STRVAL(argv[0]);
  _sv_trim(pb, 1);
  __xml_parse(ovm, dst, pb);
  _sv_space_skip(pb);
  OVM_ASSERT(_sv_eof(pb));
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
_xml_parse_bmap2(ovm_t ovm, ovm_inst_t inst, struct ovm_strval *pb)
{
  struct ovm_strval pb2[1];
  char c;
  unsigned size, sh;
  ovm_bmval_unit_t *p;

  _sv_space_skip(pb);

  *pb2 = *pb;

  for (size = 0;; ++size) {
    c = _sv_getc(pb);
    OVM_ASSERT(c != -1);
    if (c == '<') {
      _sv_ungetc(pb);
      break;
    }
    if (isspace(c)) {
      _sv_space_skip(pb);
      break;
    }
    OVM_ASSERT(c == '1' || c == '0');
  }

  OVM_ASSERT(_sv_strcmp(pb, 0, 9, "</Bitmap>"));
  
  _sv_adv(pb, 9);

  OVM_ASSERT(size > 0);

  __ovm_bmap_init(ovm, inst, size);

  for (p = BMVAL(inst)->data, sh = 0;;) {
    c = _sv_getc(pb2);
    if (c == '<')  break;
    
    *p |= (c - '0') << sh;
    if (++sh >= OVM_BMVAL_UNIT_BITS) {
      ++p;  sh = 0;
    }
  }
}

static void
_xml_parse_bmap(ovm_t ovm, ovm_inst_t inst, struct ovm_strval *pb)
{
  _sv_space_skip(pb);
  OVM_ASSERT(_sv_strcmp(pb, 0, 8, "<Bitmap>"));
  _sv_adv(pb, 8);
  _xml_parse_bmap2(ovm, inst, pb);
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
    _xml_parse_str(ovm, inst, arg, _xml_parse_bmap);
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

  __ovm_inst_method_call (ovm, &fr->work, CAR (argv[0]),
			  OVM_INST_METHOD_SEL_HASH, 0);
  h = INTVAL (fr->work);

  __ovm_inst_method_call (ovm, &fr->work, CDR (argv[0]),
			  OVM_INST_METHOD_SEL_HASH, 0);

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

static void
_xml_end_tag_find(struct ovm_strval *pb, struct ovm_strval *tb)
{
  char c;
  struct ovm_strval tbsub[1];

  for (;;) {
    c = _sv_getc(pb);
    OVM_ASSERT(c != -1);
    if (c == '<') {
      if (_sv_peek(pb, 0) == '/') {
	if (_sv_peek(pb, 1 + _sv_size(tb)) == '>' && _sv_strcmp2(pb, 1, tb)) {
	  _sv_ungetc(pb);
	  return;
	}
	OVM_ASSERT(0);
      }

      *tbsub = *pb;
      for (;;) {
	c = _sv_getc(pb);
	OVM_ASSERT(c != -1);
	if (c == '>')  break;
      }
      _sv_trim(tbsub, 1 + _sv_size(pb));

      _xml_end_tag_find(pb, tbsub);

      _sv_adv(pb, _sv_size(tbsub) + 3);
    }
  }
}

static void
_xml_parse_pair2 (ovm_t ovm, ovm_inst_t inst, struct ovm_strval *pb)
{
  struct ovm_strval tb[1], pbsub[1];

  _sv_init(tb, 4, "Pair");

  *pbsub = *pb;

  _xml_end_tag_find(pb, tb);

  _sv_trim(pbsub, _sv_size(pb));

  __xml_parse(ovm, &CAR(inst), pbsub);
  __xml_parse(ovm, &CDR(inst), pbsub);

  _sv_adv(pb, 7);
}

static void
_xml_parse_pair (ovm_t ovm, ovm_inst_t inst, struct ovm_strval *pb)
{
  _sv_space_skip (pb);
  OVM_ASSERT (_sv_strcmp (pb, 0, 6, "<Pair>"));
  _sv_adv(pb, 6);
  _xml_parse_pair2 (ovm, inst, pb);
}

static void
_ovm_pair_init(ovm_t ovm, ovm_class_t cl, ovm_inst_t inst, unsigned argc, ovm_inst_t *argv)
{
  if (argc == 1) {
    ovm_inst_t arg = argv[0];
    ovm_class_t arg_cl = ovm_inst_of(arg);

    if (arg_cl == ovm_cl_string) {
      
      
      return;
    }
    
    if (arg_cl == ovm_cl_xml) {
      _xml_parse_str(ovm, inst, arg, _xml_parse_pair);
    }

    --argc;  ++argv;
  }

  _ovm_init_parent(ovm, cl, inst, argc, argv);
}

struct ovm_class ovm_cl_pair[1] = { {
				     .name = "Pair",
				     .parent = ovm_cl_dptr,
				     .new = _ovm_inst_new1,
				     .init = _ovm_pair_init,
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
_xml_parse_list2 (ovm_t ovm, ovm_inst_t *dst, struct ovm_strval *pb)
{
  struct ovm_strval tb[1], pbsub[1];
  ovm_inst_t *p;

  OVM_FRAME_DECL(fr, work);

  _sv_init(tb, 4, "List");

  *pbsub = *pb;

  _xml_end_tag_find(pb, tb);

  _sv_trim(pbsub, _sv_size(pb));

  OVM_FRAME_ENTER(ovm, fr);

  for (p = dst;; p = &CDR(*p)) {
    _sv_space_skip(pbsub);
    if (_sv_eof(pbsub))  break;
    
    __xml_parse(ovm, &fr->work, pbsub);

    _ovm_inst_alloc(ovm, ovm_cl_list, p);
    OVM_ASSIGN(ovm, CAR(*p), fr->work);
  }

  OVM_FRAME_LEAVE(ovm);

  _sv_adv(pb, 7);
}

static void
_xml_parse_list(ovm_t ovm, ovm_inst_t *dst, struct ovm_strval *pb)
{
  _sv_space_skip (pb);
  OVM_ASSERT (_sv_strcmp (pb, 0, 6, "<List>"));
  _xml_parse_list2 (ovm, dst, pb);
}

static void
_ovm_list_new (ovm_t ovm, ovm_class_t cl, ovm_inst_t *dst,
		unsigned argc, ovm_inst_t * argv)
{
  OVM_FRAME_DECL(fr, work);

  if (argc == 1 && ovm_inst_of (argv[0]) == ovm_cl_list)
    {
      _ovm_assign (ovm, dst, argv[0]);

      return;
    }

  OVM_FRAME_ENTER(ovm, fr);

  if (argc == 1 && ovm_inst_of (argv[0]) == ovm_cl_xml) {
    struct ovm_strval pb[1];
    
    *pb = *STRVAL(argv[0]);
    _sv_trim(pb, 1);
    _xml_parse_list(ovm, &fr->work, pb);
    _sv_space_skip(pb);
    OVM_ASSERT(_sv_eof(pb));
  } else {
    OVM_ASSERT(argc < 2 || argv[1] == OVM_NIL || ovm_inst_of(argv[1]) == ovm_cl_list);
    
    _ovm_inst_alloc(ovm, ovm_cl_list, &fr->work);
    
    _ovm_init_parent(ovm, ovm_cl_list, fr->work, argc, argv);
  }

  _ovm_assign(ovm, dst, fr->work);

  OVM_FRAME_LEAVE(ovm);
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
      __ovm_inst_method_call (ovm, &fr->work, CAR (p), OVM_INST_METHOD_SEL_HASH, 0);
      h += INTVAL (fr->work);
    }

  __ovm_integer_newc (ovm, dst, h);

  OVM_FRAME_LEAVE (ovm);
}

struct ovm_class ovm_cl_list[1] = { {
				     .name = "List",
				     .parent = ovm_cl_dptr,
				     .new = _ovm_list_new,
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
_xml_parse_array2(ovm_t ovm, ovm_inst_t inst, struct ovm_strval *pb)
{
  struct ovm_strval tb[1], pbsub[1], pbsub2[1];
  unsigned size;
  ovm_inst_t *p;

  OVM_FRAME_DECL(fr, work);

  _sv_init(tb, 5, "Array");

  *pbsub = *pb;

  _xml_end_tag_find(pb, tb);

  _sv_trim(pbsub, _sv_size(pb));

  OVM_FRAME_ENTER(ovm, fr);

  *pbsub2 = *pbsub;

  for (size = 0;; ++size) {
    _sv_space_skip(pbsub);
    if (_sv_eof(pbsub))  break;
    
    __xml_parse(ovm, &fr->work, pbsub);
  }

  __ovm_array_init(ovm, inst, size);

  for (p = ARRAYVAL(inst)->data;; ++p) {
    _sv_space_skip(pbsub2);
    if (_sv_eof(pbsub2))  break;
    
    __xml_parse(ovm, p, pbsub2);
  }

  OVM_FRAME_LEAVE(ovm);

  _sv_adv(pb, 8);
}

static void
_xml_parse_array(ovm_t ovm, ovm_inst_t inst, struct ovm_strval *pb)
{
  _sv_space_skip(pb);
  OVM_ASSERT(_sv_strcmp(pb, 0, 7, "<Array>"));
  _sv_adv(pb, 7);

  _xml_parse_array2(ovm, inst, pb);
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
  else if (arg_cl == ovm_cl_xml) {
    _xml_parse_str(ovm, inst, arg, _xml_parse_array);
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

static void
_ovm_array_at(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  ovm_intval_t i;

  OVM_ASSERT(argc == 1);
  OVM_ASSERT(ovm_is_kind_of(argv[0], ovm_cl_array));
  OVM_ASSERT(ovm_is_kind_of(argv[1], ovm_cl_integer));
  i = INTVAL(argv[1]);
  OVM_ASSERT(i >= 0 && i < ARRAYVAL(argv[0])->size);

  _ovm_assign(ovm, dst, ARRAYVAL(argv[0])->data[i]);
}

void
ovm_array_atc(ovm_t ovm, ovm_inst_t *dst, ovm_inst_t rcvr, int idx)
{
  OVM_ASSERT(ovm_is_kind_of(rcvr, ovm_cl_array));
  OVM_ASSERT(idx >= 0 && idx < ARRAYVAL(rcvr)->size);

  _ovm_assign(ovm, dst, ARRAYVAL(rcvr)->data[idx]);
}

struct ovm_class ovm_cl_array[1] = { {
				      .name = "Array",
				      .parent = ovm_cl_object,
				      .new = _ovm_inst_new2,
				      .init = _ovm_array_init,
				      .walk = _ovm_array_walk,
				      .free = _ovm_array_free,
				      .inst_method_func_tbl = {
      [OVM_INST_METHOD_SEL_AT] = _ovm_array_at
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

  __ovm_inst_method_call (ovm, &fr->work, val, OVM_INST_METHOD_SEL_HASH, 0);

  b =
    &SETVAL (set)->
    base->data[INTVAL (fr->work) & (SETVAL (set)->base->size - 1)];

  for (p = b; q = *p; p = &CDR (q))
    {
      __ovm_inst_method_call (ovm, &fr->work, val,
			      OVM_INST_METHOD_SEL_EQUAL, 1, CAR (q));

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

      ++SETVAL(set)->cnt;
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
_xml_parse_set2(ovm_t ovm, ovm_inst_t inst, struct ovm_strval *pb)
{
  struct ovm_strval tb[1], pbsub[1];

  OVM_FRAME_DECL(fr, work);

  _sv_init(tb, 3, "Set");

  *pbsub = *pb;

  _xml_end_tag_find(pb, tb);

  _sv_trim(pbsub, _sv_size(pb));

  OVM_FRAME_ENTER(ovm, fr);

  for (;;) {
    _sv_space_skip(pbsub);
    if (_sv_eof(pbsub))  break;
    
    __xml_parse(ovm, &fr->work, pbsub);

    __ovm_set_put(ovm, inst, fr->work);
  }

  OVM_FRAME_LEAVE(ovm);

  _sv_adv(pb, 6);
}

static void
_xml_parse_set(ovm_t ovm, ovm_inst_t inst, struct ovm_strval *pb)
{
  _sv_space_skip(pb);
  OVM_ASSERT(_sv_strcmp(pb, 0, 5, "<Set>"));
  _sv_adv(pb, 5);

  _xml_parse_set2(ovm, inst, pb);
}

static void
_ovm_set_init (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst,
	       unsigned argc, ovm_inst_t * argv)
{
  ovm_intval_t size = OVM_SET_SIZE_DFLT;
  ovm_inst_t arg;
  ovm_class_t arg_cl = 0;

  if (argc >= 1 && ovm_inst_of(argv[0]) == ovm_cl_xml) {
    __ovm_array_init (ovm, inst, round_up_to_power_of_2 (size));
    _xml_parse_str(ovm, inst, argv[0], _xml_parse_set);

    --argc;  ++argv;

    goto done;
  }

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

 done:
  _ovm_init_parent (ovm, cl->parent, inst, argc, argv);
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

  __ovm_inst_method_call (ovm, &fr->work, key, OVM_INST_METHOD_SEL_HASH, 0);

  b =
    &DICTVAL (dict)->base->base->data[INTVAL (fr->work)
				      & (DICTVAL (dict)->base->base->size -
					 1)];

  for (p = b; q = *p; p = &CDR (q))
    {
      __ovm_inst_method_call (ovm, &fr->work, key,
			      OVM_INST_METHOD_SEL_EQUAL, 1, CAR (CAR (q)));

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

      ++DICTVAL(dict)->base->cnt;
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
_xml_parse_dict2(ovm_t ovm, ovm_inst_t inst, struct ovm_strval *pb)
{
  struct ovm_strval tb[1], pbsub[1];

  OVM_FRAME_DECL(fr, work);

  _sv_init(tb, 10, "Dictionary");

  _sv_space_skip(pb);
  *pbsub = *pb;

  _xml_end_tag_find(pb, tb);

  _sv_trim(pbsub, _sv_size(pb));

  OVM_FRAME_ENTER(ovm, fr);

  for (;;) {
    _sv_space_skip(pbsub);
    if (_sv_eof(pbsub))  break;
    
    _ovm_inst_alloc(ovm, ovm_cl_pair, &fr->work);

    _xml_parse_pair(ovm, fr->work, pbsub);

    __ovm_dict_at_put(ovm, inst, CAR(fr->work), CDR(fr->work));
  }

  OVM_FRAME_LEAVE(ovm);

  _sv_adv(pb, 13);
}

static void
_xml_parse_dict(ovm_t ovm, ovm_inst_t inst, struct ovm_strval *pb)
{
  _sv_space_skip(pb);
  OVM_ASSERT(_sv_strcmp(pb, 0, 12, "<Dictionary>"));
  _sv_adv(pb, 12);

  _xml_parse_dict2(ovm, inst, pb);
}

static void
_ovm_dict_init (ovm_t ovm, ovm_class_t cl, ovm_inst_t inst,
		unsigned argc, ovm_inst_t * argv)
{
  unsigned size = OVM_DICT_SIZE_DFLT;
  ovm_inst_t arg;
  ovm_class_t arg_cl = 0;

  if (argc >= 1 && ovm_inst_of(argv[0]) == ovm_cl_xml) {
    __ovm_array_init (ovm, inst, round_up_to_power_of_2 (size));
    _xml_parse_str(ovm, inst, argv[0], _xml_parse_dict);

    --argc;  ++argv;

    goto done;
  }

  if (argc >= 1)
    {
      arg = argv[0];
      arg_cl = ovm_inst_of (arg);

      if (arg_cl == ovm_cl_integer)
	{
	  size = INTVAL (arg);

	  OVM_ASSERT (size >= 0);
	}
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

 done:
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

#ifndef NOSTATS

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

#endif
