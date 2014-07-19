*
* $Id: longjmp.s,v 1.1.1.1 1996/02/15 17:54:20 mclareni Exp $
*
* $Log: longjmp.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:20  mclareni
* Kernlib
*
*
      .text
 #
 #CERN PROGLIB#         LONGJMP         .VERSION KERNVMI  1.01  891208
 #ORIG. 8/11/89 R.WARREN
 #
      .align      2
      .globl      longjmp_
      .loc        2 11
      .ent        longjmp_ 2
longjmp_:
      .option     O1
      subu        $sp, 32
      sw          $31, 20($sp)
      sd          $4, 32($sp)
      .mask       0x80000000, -12
      .frame      $sp, 32, $31
      .loc        2 12
      lw          $14, 36($sp)
      lw          $15, 0($14)
      sw          $15, 28($sp)
      .loc        2 13
      lw          $4, 32($sp)
      move        $5, $15
      jal         longjmp
      .loc        2 14
      lw          $31, 20($sp)
      addu        $sp, 32
      j           $31
      .end      longjmp_
#ifdef CERNLIB_CCGEN_LONGJMP
#undef CERNLIB_CCGEN_LONGJMP
#endif
#ifdef CERNLIB_TCGEN_LONGJMP
#undef CERNLIB_TCGEN_LONGJMP
#endif
