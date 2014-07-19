*
* $Id: vdot.s,v 1.1.1.1 1996/02/15 17:53:39 mclareni Exp $
*
* $Log: vdot.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:39  mclareni
* Kernlib
*
*
        TITLE   VDOT
;#
; CERN PROGLIB# F121    VDOT            .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
        HELLO (VDOT)
       SETZM
       SKIPG      ,@2(A16)
        GOODBY
       MOVEM   A01,AC01
       MOVEM   A02,AC02
       MOVEI   A01,@0(A16)
       SOS        ,A01
       HRRM    A01,LOOP
       MOVEI   A01,@1(A16)
       SOS        ,A01
       HRRM    A01,LOOP+1
       MOVEI   A02,1
LOOP:  MOVE    A01,(A02)
       FMPR    A01,(A02)
       FADR       ,A01
       CAMGE   A02,@2(A16)
       AOJA    A02,LOOP
       MOVE    A01,AC01
       MOVE   A02,AC02
        GOODBY
AC01:  0
AC02:  0
       A01= 1
       A02= 2
       A16=16
      PRGEND
#ifdef CERNLIB_TCGEN_VDOT
#undef CERNLIB_TCGEN_VDOT
#endif
