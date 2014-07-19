*
* $Id: vmatr.s,v 1.1.1.1 1996/02/15 17:53:40 mclareni Exp $
*
* $Log: vmatr.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:40  mclareni
* Kernlib
*
*
        TITLE   VMATR
;#
; CERN PROGLIB# F121    VMATR           .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
        HELLO (VMATR)
       SKIPG      ,@3(A16)
        GOODBY
       MOVEM   A01,AC01
       MOVEM   A02,AC02
       MOVEM   A03,AC03
       MOVEI      ,@2(A16)
       SOS
       HRRM       ,LOOP
       HRRM       ,LOOP1+2
       MOVEI      ,@0(A16)
       SOS
       HRRM       ,LOOP1
       MOVEI      ,@1(A16)
       SOS
       HRRM       ,LOOP1+1
       MOVEI   A01,1
LOOP:  SETZM      ,(A01)
       MOVEM   A01,A03
       MOVEI   A02,1
LOOP1: MOVE       ,(A02)
       FMPR       ,(A03)
       FADRM      ,(A01)
       ADD     A03,@4(A16)
       CAMGE   A02,@3(A16)
       AOJA    A02,LOOP1
       CAMGE   A01,@4(A16)
       AOJA    A01,LOOP
       MOVE    A01,AC01
       MOVE    A02,AC02
       MOVE    A03,AC03
        GOODBY
AC01:  0
AC02:  0
AC03:  0
       A01= 1
       A02= 2
       A03= 3
       A16=16
      PRGEND
#ifdef CERNLIB_TCGEN_VMATR
#undef CERNLIB_TCGEN_VMATR
#endif
