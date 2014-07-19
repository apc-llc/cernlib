*
* $Id: setjmp.s,v 1.1.1.1 1996/02/15 17:54:20 mclareni Exp $
*
* $Log: setjmp.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:20  mclareni
* Kernlib
*
*
      .text
 #
 #CERN PROGLIB#         SETJMP          .VERSION KERNVMI  1.01  891208
 #ORIG. 8/11/89 R.WARREN
 #
      .align      2
      .globl      setjmp_
      .loc        2 5
      .ent        setjmp_ 2
setjmp_:
      .option     O1
      j           setjmp
      sw          $4, 0($sp)
      .frame      $sp, 0, $31
      .loc        2 6
      j           $31
      .end        setjmp_
#ifdef CERNLIB_CCGEN_SETJMP
#undef CERNLIB_CCGEN_SETJMP
#endif
#ifdef CERNLIB_TCGEN_SETJMP
#undef CERNLIB_TCGEN_SETJMP
#endif
