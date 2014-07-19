*
* $Id: mxmlrt.s,v 1.1.1.1 1996/02/15 17:53:36 mclareni Exp $
*
* $Log: mxmlrt.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:36  mclareni
* Kernlib
*
*
        TITLE   MXMLRT
;#
; CERN PROGLIB# F110    MXMLRT          .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
        SEARCH HELP
;ACCUMULATOR ASSIGNMENTS:
;
;         1   IPA   2   JPA   3   II    4   JJ    5   KC,KJ,KI
;         6   IA    7   IB    10  IC    11  IA1   12  IB1
;         13  IC1   14  JA1   15  ---   16  ---   17  JA
        HELLO (MXMLRT)
        JSR     ,SAVEAC
       MOVEI   A01,1
       MOVE    A02,@4(A16)
       JRST       ,GO
        HELLO (MXMLTR)
        JSR     ,SAVEAC
       MOVE    A01,@3(A16)
       MOVEI   A02,1
GO:    SKIPG      ,@3(A16)
       JRST       ,EXIT
       SKIPG      ,@4(A16)
       JRST       ,EXIT
       MOVEI      ,@0(A16)
       SOS
       HRRM       ,LP20
       HRRM       ,LP30
       MOVEI      ,@1(A16)
       SOS
       HRRM       ,LP20+1
       MOVEI      ,@2(A16)
       SOS
       HRRM       ,LP30+1
       MOVEI   A13,1
       MOVEI   A11,1
       MOVEI   A03,1
LP50:  MOVEM   A13,A10
       MOVEI   A05,@2(A16)
       ADD     A05,A10
       SUBI    A05,2
       HRRM    A05,LP10
       MOVEI   A05,1
LP10:  SETZM      ,(A05)
       CAMGE   A05,@3(A16)
       AOJA    A05,LP10
       MOVEI   A12,1
       MOVEI   A14,1
       MOVEI   A04,1
LP40:  MOVEM   A12,A07
       MOVEM   A11,A06
       SETZM
       MOVEI   A05,1
LP20:  MOVE    A15,(A06)
       FMPR    A15,(A07)
       FADR       ,A15
       ADD     A06,A01
       ADD     A07,@4(A16)
       CAMGE   A05,@4(A16)
       AOJA    A05,LP20
       MOVEM   A14,A17
       MOVEM   A13,A10
       MOVEM      ,A15
       MOVEI   A05,1
LP30:  FMPR       ,(A17)
       FADRM      ,(A10)
       MOVEM   A15,
       AOS        ,A10
       ADD     A17,A02
       CAMGE   A05,@3(A16)
       AOJA    A05,LP30
       PAGE
       AOS        ,A12
       ADD     A14,A01
       CAMGE   A04,@4(A16)
       AOJA    A04,LP40
       ADD     A13,@3(A16)
       ADD     A11,A02
       CAMGE   A03,@3(A16)
       AOJA    A03,LP50
EXIT:  HRLZI      ,ACSAVE
       BLT        ,17
        GOODBY
SAVEAC: 0
       HRRZI      ,ACSAVE
       BLT        ,ACSAVE+17
        JRST    2,@SAVEAC
ACSAVE:BLOCK   20
       A01= 1
       A02= 2
       A03= 3
       A04= 4
       A05= 5
       A06= 6
       A07= 7
       A10=10
       A11=11
       A12=12
       A13=13
       A14=14
       A15=15
       A16=16
       A17=17
       PRGEND
#ifdef CERNLIB_TCGEN_MXMLRT
#undef CERNLIB_TCGEN_MXMLRT
#endif
