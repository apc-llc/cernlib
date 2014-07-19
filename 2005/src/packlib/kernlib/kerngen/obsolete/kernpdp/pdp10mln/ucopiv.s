*
* $Id: ucopiv.s,v 1.1.1.1 1996/02/15 17:53:37 mclareni Exp $
*
* $Log: ucopiv.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:37  mclareni
* Kernlib
*
*
        TITLE   UCOPIV
;#
; CERN PROGLIB# V301    UCOPIV          .VERSION KERNPDP  1.00  750903
;#
        SEARCH HELP
        HELLO (UCOPIV)
       SKIPG      ,@2(A16)
        GOODBY
       MOVEM   A01,AC01
       MOVEM   A02,AC02
       MOVEI   A01,1
       MOVE    A02,@2(A16)
       MOVEI      ,@0(A16)
       SOS
       HRRM       ,LOOP
       MOVEI      ,@1(A16)
       SOS
       HRRM       ,LOOP+1
LOOP:  MOVE       ,(A01)
       MOVEM      ,(A02)
       SOS        ,A02
       CAMGE   A01,@2(A16)
       AOJA    A01,LOOP
       MOVE    A01,AC01
       MOVE    A02,AC02
        GOODBY
       A01= 1
       A16=16
       A02= 2
AC01:  0
AC02:  0
      PRGEND
#ifdef CERNLIB_TCGEN_UCOPIV
#undef CERNLIB_TCGEN_UCOPIV
#endif
