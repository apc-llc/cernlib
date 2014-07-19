*
* $Id: iufind.s,v 1.1.1.1 1996/02/15 17:53:34 mclareni Exp $
*
* $Log: iufind.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:34  mclareni
* Kernlib
*
*
        TITLE   IUFIND
;#
; CERN PROGLIB# V304    IUFIND          .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
        HELLO (IUFIND)
       MOVEM   A01,AC01
       MOVEI      ,@1(A16)
       SOS
       HRRM       ,LOOP
       MOVE    A01,@2(A16)
LOOP:  MOVE       ,(A01)
       CAMN       ,@0(A16)
       JRST       ,P24
       CAMGE   A01,@3(A16)
       AOJA    A01,LOOP
       MOVE       ,@3(A16)
       AOS
       MOVE    A01,AC01
       GOODBY
P24:   MOVE       ,A01
       MOVE    A01,AC01
        GOODBY
AC01:  0
       A01= 1
       A16=16
      PRGEND
#ifdef CERNLIB_TCGEN_IUFIND
#undef CERNLIB_TCGEN_IUFIND
#endif
