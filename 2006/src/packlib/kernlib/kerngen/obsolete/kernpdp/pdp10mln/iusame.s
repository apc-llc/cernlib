*
* $Id: iusame.s,v 1.1.1.1 1996/02/15 17:53:36 mclareni Exp $
*
* $Log: iusame.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:36  mclareni
* Kernlib
*
*
        TITLE   IUSAME
;#
; CERN PROGLIB# M501    IUSAME          .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
        HELLO (IUSAME)
       MOVEM   A01,AC01
       MOVEM   A02,AC02
       MOVEM   A03,AC03
       MOVEI      ,@0(A16)
       SOS
       HRRM       ,SET
       SOS
       HRRM       ,SET+1
       MOVE    A01,@1(A16)
P21:   SETZM      ,A02
       MOVE    A03,A01
P24:   AOS        ,A02
       CAMN    A01,@2(A16)
       JRST       ,P31
       AOS        ,A01
SET:   MOVE       ,(A01)
       CAMN       ,(A01)
       JRST       ,P24
       CAMGE   A02,@3(A16)
       JRST       ,P21
       JRST       ,P32
P31:   CAML    A02,@3(A16)
       JRST       ,P32
       SETZM      ,A02
       MOVE    A03,@2(A16)
       AOS        ,A03
P32:   MOVE       ,A02
       MOVEM   A03,@4(A16)
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
#ifdef CERNLIB_TCGEN_IUSAME
#undef CERNLIB_TCGEN_IUSAME
#endif
