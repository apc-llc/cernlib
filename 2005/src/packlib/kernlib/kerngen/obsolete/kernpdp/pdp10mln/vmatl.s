*
* $Id: vmatl.s,v 1.1.1.1 1996/02/15 17:53:39 mclareni Exp $
*
* $Log: vmatl.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:39  mclareni
* Kernlib
*
*
        TITLE   VMATL
;#
; CERN PROGLIB# F121    VMATL           .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
        HELLO (VMATL)
       SKIPG      ,@4(A16)
        GOODBY
       MOVEM   A01,AC01
       MOVEM   A02,AC02
       MOVEM   A03,AC03
       MOVEI      ,@2(A16)
       SOS
       HRRM       ,LOOP
       HRRM       ,LOOP1+2
       MOVEI      ,@0(16)
       SOS
       HRRM       ,LOOP1
       MOVEI      ,@1(A16)
       SOS
       HRRM       ,LOOP1+1
       MOVEI   A01,1
       MOVEI   A03,1
       PAGE
LOOP:  SETZM      ,(A01)
       MOVEI   A02,1
LOOP1: MOVE       ,(A03)
       FMPR       ,(A02)
       FADRM      ,(A01)
       AOS        ,A03
       CAMGE   A02,@4(A16)
       AOJA    A02,LOOP1
       CAMGE   A01,@3(A16)
       AOJA    A01,LOOP
       MOVE    A01,AC01
       MOVE    A02,AC02
       MOVE    A03,AC03
        GOODBY
       A01= 1
       A02= 2
       A03= 3
       A16=16
AC01:  0
AC02:  0
AC03:  0
      PRGEND
#ifdef CERNLIB_TCGEN_VMATL
#undef CERNLIB_TCGEN_VMATL
#endif
