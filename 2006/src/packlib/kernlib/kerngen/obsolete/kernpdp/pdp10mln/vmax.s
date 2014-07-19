*
* $Id: vmax.s,v 1.1.1.1 1996/02/15 17:53:40 mclareni Exp $
*
* $Log: vmax.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:40  mclareni
* Kernlib
*
*
        TITLE   VMAX
;#
; CERN PROGLIB# F121    VMAX            .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH   HELP
        HELLO (VMAX)
       MOVEM   A01,AC01
       MOVEI   A01,0
       JRST       ,GO
        HELLO (VMAXA)
       MOVEM   A01,AC01
       MOVEI   A01,1
       JRST       ,GO
        HELLO (VMIN)
       MOVEM   A01,AC01
       MOVEI   A01,2
       JRST       ,GO
        HELLO (VMINA)
       MOVEM   A01,AC01
       MOVEI   A01,3
       JRST       ,GO
GO:    SETZM
       SKIPG      ,@1(A16)
       JRST       ,EXIT
       MOVEM   A02,AC02
       MOVEM   A03,AC03
       MOVEI   A02,3
       XCT        ,COM1(A01)
       MOVEI   A03,@0(A16)
       SOS        ,A03
       HRRM    A03,COM2(A02)
       SOJGE   A02,.-1
       MOVEI   A02,1
       PAGE
LOOP:  XCT        ,COM2(A01)
       XCT        ,COM3(A01)
       MOVEM   A03,
       CAMGE   A02,@1(A16)
       AOJA    A02,LOOP
       MOVE    A02,AC02
       MOVE    A03,AC03
EXIT:  MOVE    A01,AC01
        GOODBY
COM1:  MOVE       ,@0(A16)
       SETZM
       MOVE       ,@0(A16)
       MOVM       ,@0(A16)
COM2:  MOVE    A03,(A02)
       MOVM    A03,(A02)
       MOVE    A03,(A02)
       MOVM    A03,(A02)
COM3:  CAMLE   A03,
       CAMLE   A03,
       CAMGE   A03,
       CAMGE   A03,
AC01:  0
AC02:  0
AC03:  0
       A01= 1
       A02= 2
       A03= 3
       A16=16
      PRGEND
#ifdef CERNLIB_TCGEN_VMAX
#undef CERNLIB_TCGEN_VMAX
#endif
#ifdef CERNLIB_TCGEN_VMAXA
#undef CERNLIB_TCGEN_VMAXA
#endif
#ifdef CERNLIB_TCGEN_VMIN
#undef CERNLIB_TCGEN_VMIN
#endif
#ifdef CERNLIB_TCGEN_VMINA
#undef CERNLIB_TCGEN_VMINA
#endif
