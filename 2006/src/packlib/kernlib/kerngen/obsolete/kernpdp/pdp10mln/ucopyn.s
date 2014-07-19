*
* $Id: ucopyn.s,v 1.1.1.1 1996/02/15 17:53:37 mclareni Exp $
*
* $Log: ucopyn.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:37  mclareni
* Kernlib
*
*
        TITLE   UCOPYN
;#
; CERN PROGLIB# V301    UCOPYN          .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
        HELLO (UCOPYN)
       SKIPG      ,@2(A16)
         GOODBY
       MOVEM   A01,AC01
       MOVEI      ,@0(A16)
       SOS
       HRRM       ,LOOP
       MOVEI      ,@1(A16)
       SOS
       HRRM       ,LOOP+1
       MOVEI   A01,1
LOOP:  MOVN       ,(A01)
       MOVEM      ,(A01)
       CAMGE   A01,@2(A16)
       AOJA    A01,LOOP
       MOVE    A01,AC01
        GOODBY
       A01= 1
       A16=16
AC01:  0
      PRGEND
#ifdef CERNLIB_TCGEN_UCOPYN
#undef CERNLIB_TCGEN_UCOPYN
#endif
