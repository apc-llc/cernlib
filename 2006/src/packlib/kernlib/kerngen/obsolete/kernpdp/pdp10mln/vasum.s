*
* $Id: vasum.s,v 1.1.1.1 1996/02/15 17:53:38 mclareni Exp $
*
* $Log: vasum.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:38  mclareni
* Kernlib
*
*
        TITLE   VASUM
;#
; CERN PROGLIB# F121    VASUM           .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
        HELLO (VASUM)
       SETZM
       SKIPG      ,@1(A16)
        GOODBY
       MOVEM   A01,AC01
       MOVEM   A02,AC02
       MOVEI   A01,@0(A16)
       SOS        ,A01
       HRRM    A01,LOOP
       MOVEI   A02,1
LOOP:  MOVM    A01,(A02)
       FADR       ,A01
       CAMGE   A02,@1(A16)
       AOJA    A02,LOOP
       MOVE    A01,AC01
       MOVE    A02,AC02
        GOODBY
AC01: 0
AC02: 0
       A01= 1
       A02= 2
       A16=16
      PRGEND
#ifdef CERNLIB_TCGEN_VASUM
#undef CERNLIB_TCGEN_VASUM
#endif
