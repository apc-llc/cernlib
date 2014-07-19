*
* $Id: vline.s,v 1.1.1.1 1996/02/15 17:53:39 mclareni Exp $
*
* $Log: vline.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:39  mclareni
* Kernlib
*
*
        TITLE   VLINE
;#
; CERN PROGLIB# F121    VLINE           .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
        HELLO (VLINE)
       SKIPG      ,@5(A16)
        GOODBY
       MOVEI      ,@4(A16)
       SOS
       HRRM       ,LOOP+5
       MOVEI      ,@0(A16)
       SOS
       HRRM       ,LOOP+2
       MOVEI      ,@2(A16)
       SOS
       HRRM       ,LOOP
       MOVEM   A01,AC01
       MOVEM   A02,AC02
       MOVEI   A02,1
LOOP:  MOVE       ,(A02)
       FMPR       ,@3(A16)
       MOVE    A01,(A02)
       FMPR    A01,@1(A16)
       FADR       ,A01
       MOVEM      ,(A02)
       CAMGE   A02,@5(A16)
       AOJA    A02,LOOP
       MOVE    A01,AC01
       MOVE    A02,AC02
        GOODBY
AC01:  0
AC02:  0
       A01= 1
       A02= 2
       A16=16
      PRGEND
#ifdef CERNLIB_TCGEN_VLINE
#undef CERNLIB_TCGEN_VLINE
#endif
