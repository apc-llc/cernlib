*
* $Id: lvmima.s,v 1.1.1.1 1996/02/15 17:53:36 mclareni Exp $
*
* $Log: lvmima.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:36  mclareni
* Kernlib
*
*
        TITLE   LVMIMA
;#
; CERN PROGLIB# F121    LVMINA          .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
        HELLO (LVMAX)
       MOVEM   A01,AC01
       MOVEI   A01,0
       JRST       ,GO
        HELLO (LVMAXA)
       MOVEM   A01,AC01
       MOVEI   A01,1
       JRST       ,GO
        HELLO (LVMIN)
       MOVEM   A01,AC01
       MOVEI   A01,2
       JRST       ,GO
        HELLO (LVMINA)
       MOVEM   A01,AC01
       MOVEI   A01,3
       JRST       ,GO
GO:    SKIPG      ,@1(A16)
       JRST       ,P100
       MOVEM   A02,AC02
       MOVEM   A03,AC03
       MOVEM   A04,AC04
       MOVEI   A02,1
       XCT        ,COM1(A01)
       MOVEI   A03,3
       MOVEI      ,@0(A16)
       SOS
       HRRM       ,COM2(A03)
       SOJGE   A03,.-1
       MOVEI   A03,1
LOOP:  XCT        ,COM2(A01)
       XCT        ,COM3(A01)
       JRST       ,P9
       MOVEM   A04,
       MOVEM   A03,A02
       PAGE
P9:    CAMGE   A03,@1(A16)
       AOJA    A03,LOOP
       MOVEM   A02,
       MOVE    A01,AC01
       MOVE    A02,AC02
       MOVE    A03,AC03
       MOVE    A04,AC04
        GOODBY
P100:  SETZM
       MOVE    A01,AC01
        GOODBY
COM1:  MOVE       ,@0(A16)
       SETZM
       MOVE       ,@0(A16)
       MOVM       ,@0(A16)
COM2:  MOVE    A04,(A03)
       MOVM    A04,(A03)
       MOVE    A04,(A03)
       MOVM    A04,(A03)
COM3:  CAMG    A04,
       CAMG    A04,
       CAMLE   A04,
       CAMLE   A04,
       A01= 1
       A02= 2
       A03= 3
       A04= 4
       A16=16
AC01:  0
AC02:  0
AC03:  0
AC04:  0
      PRGEND
#ifdef CERNLIB_TCGEN_LVMAX
#undef CERNLIB_TCGEN_LVMAX
#endif
#ifdef CERNLIB_TCGEN_LVMAXA
#undef CERNLIB_TCGEN_LVMAXA
#endif
#ifdef CERNLIB_TCGEN_LVMIN
#undef CERNLIB_TCGEN_LVMIN
#endif
#ifdef CERNLIB_TCGEN_LVMINA
#undef CERNLIB_TCGEN_LVMINA
#endif
