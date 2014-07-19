*
* $Id: vsum.s,v 1.1.1.1 1996/02/15 17:53:41 mclareni Exp $
*
* $Log: vsum.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:41  mclareni
* Kernlib
*
*
        TITLE   VSUM
;#
; CERN PROGLIB# F121    VSUM            .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
        HELLO (VSUM)
       SETZM
       SKIPG      ,@1(A16)
        GOODBY
       MOVEM   A01,AC01
       MOVEI   A01,@0(A16)
       SOS        ,A01
       HRRM    A01,LOOP
       MOVEI   A01,1
LOOP:  FADR       ,(A01)
       CAMGE   A01,@1(A16)
       AOJA    A01,LOOP
       MOVE    A01,AC01
        GOODBY
AC01:  0
       A01= 1
       A16=16
      PRGEND
#ifdef CERNLIB_TCGEN_VSUM
#undef CERNLIB_TCGEN_VSUM
#endif
