#include "ovm.h"

#include <stdio.h>

enum
{
  OVM_INST_PAGE_SIZE = 4096
};


OVM_FRAME_DECL (glob, work[16], foo[1]);

struct ovm ovm[1];



void
inst_print (ovm_t ovm, ovm_inst_t inst)
{
  union ovm_cval cv[1];
  OVM_FRAME_DECL (fr, work);

  if (ovm_is_kind_of (inst, ovm_cl_string))
    {
      ovm_cval_get (ovm, cv, inst);

      printf ("%s", cv->strval->data);
    }
  else
    {
      OVM_FRAME_ENTER (ovm, fr);

      OVM_INST_NEW (ovm, fr->work, ovm_cl_string, inst);
      ovm_cval_get (ovm, cv, fr->work);

      printf ("%s", cv->strval->data);

      OVM_FRAME_LEAVE (ovm);
    }
}


int
main (void)
{
  ovm_init (ovm, OVM_INST_PAGE_SIZE);

  OVM_FRAME_ENTER (ovm, glob);

#if 0

  unsigned n;

  OVM_INTEGER_NEWC (ovm, glob->work[0], 0);
  OVM_INTEGER_NEWC (ovm, glob->work[1], 1);

  for (n = 1000000000; n; --n)
    {
      OVM_INST_METHOD_CALL (ovm, glob->work[0], glob->work[0],
			    OVM_INST_METHOD_SEL_ADD, glob->work[1]);
    }

#endif

#if 0

  OVM_BOOL_NEWC (ovm, glob->work[0], 1);

  OVM_INST_METHOD_CALL (ovm, glob->work[1],
			glob->work[0], OVM_INST_METHOD_SEL_NOT);

  OVM_INST_NEW (ovm, glob->work[2], ovm_cl_integer, glob->work[0]);

  OVM_INST_NEW (ovm, glob->work[3], ovm_cl_pair,
		glob->work[0], glob->work[2]);

  OVM_STRING_NEWC (ovm, glob->work[2], "The rain in Spain");

  OVM_INTEGER_NEWC (ovm, glob->work[3], 42);

  OVM_INST_NEW (ovm, glob->work[2], ovm_cl_integer, glob->work[3]);

  OVM_INST_NEW (ovm, glob->work[3], ovm_cl_string, glob->work[3]);

#endif

#if 0

  ovm_inst_t foo;

  OVM_INTEGER_NEWC (ovm, foo, 42);

#endif

#if 0

  union ovm_cval cv[1];

  OVM_INTEGER_NEWC (ovm, glob->work[0], 42);

  OVM_INST_NEW (ovm, glob->work[0], ovm_cl_bmap, glob->work[0]);

  OVM_INTEGER_NEWC (ovm, glob->work[1], 9);
  OVM_INTEGER_NEWC (ovm, glob->work[2], 32);
  OVM_INTEGER_NEWC (ovm, glob->work[3], 0x12345678);

  OVM_INST_NEW (ovm, glob->work[4], ovm_cl_xml, glob->work[2]);

  inst_print (ovm, glob->work[4]);

  OVM_INST_METHOD_CALL (ovm, glob->work[4], glob->work[0],
			OVM_INST_METHOD_SEL_AT_LEN_PUT, glob->work[1],
			glob->work[2], glob->work[3]);

  inst_print (ovm, glob->work[4]);

  OVM_INST_NEW (ovm, glob->work[5], ovm_cl_xml, glob->work[4]);

  inst_print (ovm, glob->work[5]);

  OVM_INTEGER_NEWC (ovm, glob->work[1], 11);
  OVM_INTEGER_NEWC (ovm, glob->work[2], 7);

  OVM_INST_METHOD_CALL (ovm, glob->work[5], glob->work[4],
			OVM_INST_METHOD_SEL_AT_LEN, glob->work[1],
			glob->work[2]);

  OVM_INST_NEW (ovm, glob->work[8], ovm_cl_pair, glob->work[1],
		glob->work[2]);

  OVM_INST_NEW (ovm, glob->work[9], ovm_cl_xml, glob->work[8]);

  inst_print (ovm, glob->work[9]);

#endif

#if 0

  OVM_XML_NEWC (ovm, glob->work[0], "   <Integer>     42   </Integer>  ");

  OVM_INST_NEW (ovm, glob->work[1], ovm_cl_integer, glob->work[0]);

#endif

#if 0

  OVM_STRING_NEWC (ovm, glob->work[0], "The rain in Spain's plain is wet");

  OVM_INST_NEW (ovm, glob->work[1], ovm_cl_xml, glob->work[0]);

  inst_print (ovm, glob->work[1]);


#endif

#if 0

  OVM_XML_NEWC (ovm, glob->work[0], "   <Integer>     42   </Integer>  ");

  OVM_INST_METHOD_CALL (ovm, glob->work[1], glob->work[0], OVM_INST_METHOD_SEL_PARSE);

  inst_print(ovm, glob->work[1]);

#endif

#if 1

  OVM_XML_NEWC(ovm, glob->work[0], "<Pair>   <Pair> <Integer>    42 </Integer> <Boolean>T</Boolean> </Pair>  <String>foo</String>    </Pair>  ");

  OVM_INST_METHOD_CALL (ovm, glob->work[1], glob->work[0], OVM_INST_METHOD_SEL_PARSE);

  inst_print(ovm, glob->work[1]);

  printf("\n");

  OVM_XML_NEWC(ovm, glob->work[0], "<List>   <Pair> <Integer>    42 </Integer> <Boolean>T</Boolean> </Pair>  <String>foo</String>   <Float>3.1415926 </Float>   </List>  ");

  OVM_INST_METHOD_CALL (ovm, glob->work[1], glob->work[0], OVM_INST_METHOD_SEL_PARSE);

  inst_print(ovm, glob->work[1]);

  printf("\n");

  OVM_XML_NEWC(ovm, glob->work[0], "<Array>   <Pair> <Integer>    42 </Integer> <Boolean>T</Boolean> </Pair>  <String>foo</String>   <Float>3.1415926 </Float>   </Array>  ");

  OVM_INST_METHOD_CALL (ovm, glob->work[1], glob->work[0], OVM_INST_METHOD_SEL_PARSE);

  inst_print(ovm, glob->work[1]);

  printf("\n");

  OVM_XML_NEWC(ovm, glob->work[0], "<Dictionary>   <Pair> <Integer>    42 </Integer> <Boolean>T</Boolean> </Pair>  <Pair> <String>foo</String>   <Float>3.1415926 </Float>   </Pair></Dictionary>  ");

  OVM_INST_METHOD_CALL (ovm, glob->work[1], glob->work[0], OVM_INST_METHOD_SEL_PARSE);

  inst_print(ovm, glob->work[1]);

  printf("\n");

#endif

  OVM_FRAME_LEAVE (ovm);

#ifndef NOSTATS
  ovm_stats_print (ovm);
#endif

  return (0);
}
