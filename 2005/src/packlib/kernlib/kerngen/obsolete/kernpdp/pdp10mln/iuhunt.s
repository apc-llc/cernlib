*
* $Id: iuhunt.s,v 1.1.1.1 1996/02/15 17:53:36 mclareni Exp $
*
* $Log: iuhunt.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:36  mclareni
* Kernlib
*
*
        TITLE   IUHUNT
;#
; CERN PROGLIB# V304    IUHUNT          .VERSION KERNPDP  1.02  761001
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
        HELLO (IUHUNT)
       SKIPG   A01,@2(A16)
       JRST       ,EXIT
       MOVEI   1,1
       MOVEI      ,@1(A16)
       SOS
       HRRM       ,LOOP
LOOP:  MOVE       ,(A01)
       CAMN       ,@0(A16)
       JRST       ,P21
       CAML    A01,@2(A16)
       JRST       ,EXIT
       ADD     A01,@3(A16)
       JRST       ,LOOP
EXIT:   MOVE    A01,AC01
       SETZM
        GOODBY
P21:   MOVEM   A01,
        GOODBY
AC01:  0
       A01= 1
       A16=16
      PRGEND
#ifdef CERNLIB_TCGEN_IUHUNT
#undef CERNLIB_TCGEN_IUHUNT
#endif
