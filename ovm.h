
#define ARRAY_SIZE(a)  (sizeof(a) / sizeof((a)[0]))

#define PTR_TO_INT(x)                     ((long long)(x))
#define FIELD_OFS(s, f)                   (PTR_TO_INT(&((s *) 0)->f))
#define FIELD_PTR_TO_STRUCT_PTR(p, s, f)  ((s *)((char *)(p) - FIELD_OFS(s, f)))

#ifndef NDEBUG
#define OVM_DEBUG_ASSERT(_x)  assert(_x)
#else
#define OVM_DEBUG_ASSERT(_x)
#endif
#define OVM_ASSERT(_x)  assert(_x)


struct list
{
  struct list *prev, *next;
};

static inline struct list *
list_init (struct list *li)
{
  return (li->prev = li->next = li);
}

static inline struct list *
list_first (struct list *li)
{
  return (li->next);
}

static inline struct list *
list_last (struct list *li)
{
  return (li->prev);
}

static inline struct list *
list_end (struct list *li)
{
  return (li);
}

static inline unsigned
list_empty (struct list *li)
{
  return (list_first (li) == list_end (li));
}

static inline struct list *
list_next (struct list *li)
{
  return (li->next);
}

static inline struct list *
list_prev (struct list *li)
{
  return (li->prev);
}

static inline struct list *
list_insert (struct list *nd, struct list *before)
{
  struct list *p;

  p = before->prev;

  nd->prev = p;
  nd->next = before;

  return (p->next = before->prev = nd);
}

static inline struct list *
list_erase (struct list *nd)
{
  struct list *p, *q;

  p = nd->prev;
  q = nd->next;

  p->next = q;
  q->prev = p;

#ifndef NDEBUG
  nd->prev = nd->next = 0;
#endif

  return (nd);
}




struct ovm;
struct ovm_class;
struct ovm_inst;

typedef struct ovm_class *ovm_class_t;
typedef struct ovm_inst  *ovm_inst_t;
typedef struct ovm       *ovm_t;

#define OVM_NIL  ((ovm_inst_t) 0)

struct ovm_inst_page;

typedef unsigned char ovm_boolval_t;
typedef long long     ovm_intval_t;
typedef long double   ovm_floatval_t;
typedef void          *ovm_refval_t;
typedef unsigned      ovm_bmval_unit_t;
enum {
  OVM_BMVAL_UNIT_BITS_LOG2 = 5,
  OVM_BMVAL_UNIT_BITS      = 1 << OVM_BMVAL_UNIT_BITS_LOG2
};


struct ovm_inst
{
#ifndef NDEBUG
  unsigned long long magic;
#endif
  struct list list_node[1];
  struct ovm_inst_page *inst_page;
  unsigned ref_cnt;
  ovm_class_t inst_of;
  union
  {
    ovm_boolval_t boolval;
#define BOOLVAL(_x)  ((_x)->val->boolval)
    ovm_intval_t intval;
#define INTVAL(_x)  ((_x)->val->intval)
    ovm_floatval_t floatval;
#define FLOATVAL(_x)  ((_x)->val->floatval)
    ovm_refval_t refval;
#define REFVAL(_x)  ((_x)->val->refval)
    struct ovm_strval
    {
      unsigned size;
      char *data;
    } strval[1];
#define STRVAL(_x)  ((_x)->val->strval)
    struct ovm_bmval
    {
      unsigned         size;
      ovm_bmval_unit_t *data;
    } bmval[1];
#define BMVAL(_x)  ((_x)->val->bmval)
    struct ovm_dptrval
    {
      ovm_inst_t car, cdr;
    } dptrval[1];
#define CAR(_x)  ((_x)->val->dptrval->car)
#define CDR(_x)  ((_x)->val->dptrval->cdr)
    struct ovm_arrayval
    {
      unsigned size;
      ovm_inst_t *data;
    } arrayval[1];
#define ARRAYVAL(_x)  ((_x)->val->arrayval)
    struct ovm_setval
    {
      struct ovm_arrayval base[1];
      unsigned cnt;
#define OVM_SET_SIZE_DFLT  32
    } setval[1];
#define SETVAL(_x)  ((_x)->val->setval)
    struct ovm_dictval
    {
      struct ovm_setval base[1];
    } dictval[1];
#define DICTVAL(_x)  ((_x)->val->dictval)
  } val[1];
};

struct ovm_inst_page
{
  struct list list_node[1];
  unsigned in_use_cnt;
};

enum
{
  OVM_INST_METHOD_SEL_ADD,
  OVM_INST_METHOD_SEL_AND,
  OVM_INST_METHOD_SEL_AT,
  OVM_INST_METHOD_SEL_AT_LEN,
  OVM_INST_METHOD_SEL_AT_LEN_PUT,
  OVM_INST_METHOD_SEL_AT_PUT,
  OVM_INST_METHOD_SEL_CAR,
  OVM_INST_METHOD_SEL_CDR,
  OVM_INST_METHOD_SEL_DIV,
  OVM_INST_METHOD_SEL_EQUAL,
  OVM_INST_METHOD_SEL_HASH,
  OVM_INST_METHOD_SEL_IN,
  OVM_INST_METHOD_SEL_MULT,
  OVM_INST_METHOD_SEL_NOT,
  OVM_INST_METHOD_SEL_PUT,
  OVM_INST_METHOD_SEL_SIZE,
  OVM_INST_METHOD_SEL_SUB,
  OVM_INST_METHOD_SEL_OR,
  OVM_INST_METHOD_SEL_XOR,
  OVM_INST_METHOD_NUM_SELS
};

typedef void (*ovm_inst_method_t) (struct ovm *, ovm_inst_t *, unsigned,
				   ovm_inst_t *);

struct ovm_class
{
  char *name;
  ovm_class_t parent;
  void (*new) (struct ovm * ovm, ovm_class_t cl, ovm_inst_t *dst,
		unsigned argc, ovm_inst_t * argv);
  void (*init) (struct ovm * ovm, ovm_class_t cl, ovm_inst_t inst,
		unsigned argc, ovm_inst_t * argv);
  void (*walk) (struct ovm * ovm, ovm_class_t cl, ovm_inst_t inst,
		void (*func) (struct ovm *, ovm_inst_t));
  void (*free) (struct ovm * ovm, ovm_class_t cl, ovm_inst_t inst);
  ovm_inst_method_t inst_method_func_tbl[OVM_INST_METHOD_NUM_SELS];
};

struct ovm_class ovm_cl_object[1];
struct ovm_class ovm_cl_bool[1];
struct ovm_class ovm_cl_num[1];
struct ovm_class ovm_cl_integer[1];
struct ovm_class ovm_cl_float[1];
struct ovm_class ovm_cl_ref[1];
struct ovm_class ovm_cl_string[1];
struct ovm_class ovm_cl_xml[1];
struct ovm_class ovm_cl_bmap[1];
struct ovm_class ovm_cl_dptr[1];
struct ovm_class ovm_cl_pair[1];
struct ovm_class ovm_cl_list[1];
struct ovm_class ovm_cl_array[1];
struct ovm_class ovm_cl_set[1];
struct ovm_class ovm_cl_dict[1];

inline ovm_class_t
ovm_inst_of (ovm_inst_t inst)
{
  return (inst ? inst->inst_of : ovm_cl_object);
}




struct ovm_frame
{
  struct ovm_frame *prev;
  unsigned   size;		/* In insts */
#ifndef NDEBUG
  ovm_inst_t *start, *end;
#endif
};

struct ovm
{
  unsigned inst_page_size;
  unsigned insts_per_page;

  struct list inst_pages[1];
  struct list insts_free[1];
  struct list insts_in_use[1];

  struct ovm_frame *frp;

  struct {
    unsigned long long insts_in_use, insts_max;
    unsigned long long pages_in_use, pages_max;
    unsigned long long mem_in_use, mem_max;
  } stats[1];
};

#define OVM_FRAME_DECL(_nm, ...)  struct { struct ovm_frame hdr[1]; ovm_inst_t __VA_ARGS__; } _nm [1]

void _ovm_frame_enter (ovm_t ovm, struct ovm_frame *fr, unsigned size);
#define OVM_FRAME_ENTER(_ovm, _fr)  (_ovm_frame_enter((_ovm), (_fr)->hdr, sizeof(_fr)))

void _ovm_frame_leave (ovm_t ovm);
#define OVM_FRAME_LEAVE(_ovm)  (_ovm_frame_leave(_ovm))

struct ovm *ovm_init (ovm_t ovm, unsigned inst_page_size);

void _ovm_assign (ovm_t ovm, ovm_inst_t * dst, ovm_inst_t src);
#define OVM_ASSIGN(_ovm, _dst, _src)  (_ovm_assign((_ovm), &(_dst), (_src)))

void _ovm_inst_new (ovm_t ovm, ovm_inst_t * dst, ovm_class_t cl,
		    unsigned argc, ...);
#define OVM_INST_NEW(_ovm, _dst, _cl, ...)  (_ovm_inst_new((_ovm), &(_dst), (_cl), (sizeof((ovm_inst_t[]){0, __VA_ARGS__}) / sizeof(ovm_inst_t)) - 1, ## __VA_ARGS__))

void _ovm_inst_method_call (ovm_t ovm, ovm_inst_t * dst,
			    ovm_inst_t rcvr, unsigned sel, unsigned argc,
			    ...);
#define OVM_INST_METHOD_CALL(_ovm, _dst, _rcvr, _sel, ...)  (_ovm_inst_method_call((_ovm), &(_dst), (_rcvr), (_sel), (sizeof((ovm_inst_t[]){0, __VA_ARGS__}) / sizeof(ovm_inst_t)) - 1, ## __VA_ARGS__))
void _ovm_inst_method_call_cl (ovm_t ovm, ovm_inst_t * dst,
			       ovm_inst_t rcvr, ovm_class_t cl, unsigned sel,
			       unsigned argc, ...);
#define OVM_INST_METHOD_CALL_CL(_ovm, _dst, _rcvr, _cl, _sel, ...)  (_ovm_inst_method_call_cl((_ovm), &(_dst), (_rcvr), (_cl), (_sel), (sizeof((ovm_inst_t[]){0, __VA_ARGS__}) / sizeof(ovm_inst_t)) - 1, ## __VA_ARGS__))

void _ovm_bool_newc (ovm_t ovm, ovm_inst_t * dst, ovm_boolval_t val);
#define OVM_BOOL_NEWC(_ovm, _dst, _val)  (_ovm_bool_newc((_ovm), &(_dst), (_val)))

void _ovm_integer_newc (ovm_t ovm, ovm_inst_t * dst, ovm_intval_t val);
#define OVM_INTEGER_NEWC(_ovm, _dst, _val)  (_ovm_integer_newc((_ovm), &(_dst), (_val)))

void ovm_float_newc (ovm_t ovm, ovm_inst_t * dst, ovm_floatval_t val);
#define OVM_FLOAT_NEWC(_ovm, _dst, _val)  (_ovm_float_newc((_ovm), &(_dst), (_val)))

static struct ovm_strval *__ovm_strval_initv (ovm_t ovm, struct ovm_strval *dst, unsigned argc, struct ovm_strval *argv);
static struct ovm_strval *__ovm_strval_initc (ovm_t ovm, struct ovm_strval *dst, unsigned argc, ...);
void _ovm_string_newc (ovm_t ovm, ovm_inst_t * dst, char *val);
#define OVM_STRING_NEWC(_ovm, _dst, _val)  (_ovm_string_newc((_ovm), &(_dst), (_val)))

void _ovm_xml_newc (ovm_t ovm, ovm_inst_t * dst, char *val);
#define OVM_XML_NEWC(_ovm, _dst, _val)  (_ovm_xml_newc((_ovm), &(_dst), (_val)))

void _ovm_array_newc (ovm_t ovm, ovm_inst_t * dst, unsigned size);
#define OVM_ARRAY_NEWC(_ovm, _dst, _size)  (_ovm_array_newc((_ovm), &(_dst), (_size)))
unsigned ovm_array_size (ovm_t ovm, ovm_inst_t arr);
void _ovm_array_atc (ovm_t ovm, ovm_inst_t * dst, ovm_inst_t rcvr,
		     unsigned idx);
#define OVM_ARRAY_ATC(_ovm, _dst, _idx)  (_ovm_array_atc((_ovm), &(_dst), (_idx)))
void _ovm_array_atc_put (ovm_t ovm, ovm_inst_t * dst, ovm_inst_t rcvr,
			 unsigned idx, ovm_inst_t val);
#define OVM_ARRAY_ATC_PUT(_ovm, _dst, _idx, _val)  (_ovm_array_atc_put((_ovm), &(_dst), (_idx), (_val)))


union ovm_cval
{
  ovm_boolval_t     boolval;
  ovm_intval_t      intval;
  ovm_floatval_t    floatval;
  struct {
    unsigned   size;
    const char *data;
  } strval[1];
  struct {
    unsigned               size;
    const ovm_bmval_unit_t *data;
  } bmval[1];    
};
typedef union ovm_cval ovm_cval_t[1];

void ovm_cval_get (ovm_t ovm, ovm_cval_t dst, ovm_inst_t inst);

void ovm_stats_print(ovm_t ovm);
