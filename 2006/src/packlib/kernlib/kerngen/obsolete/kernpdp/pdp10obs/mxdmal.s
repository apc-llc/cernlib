*
* $Id: mxdmal.s,v 1.1.1.1 1996/02/15 17:53:47 mclareni Exp $
*
* $Log: mxdmal.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:47  mclareni
* Kernlib
*
*
        TITLE   MXDMAL
        SUBTTL  P. SCHMITZ
        SEARCH HELP
        HELLO (MXDMAL)
       SETZM
       JRST       ,GO                   ;   SUBROUTINE MXDMAL (A,B,R,I,J
        HELLO (MXDMAR)
       MOVEI      ,1                    ;   IF (I.EQ.0.OR.J.EQ.0) RETURN
       JRST       ,GO                   ;   IB = 1
        HELLO (MXDML)
       MOVEI      ,2                    ;   R(IB) = R(IB)+B(IB)*A(L)
       JRST       ,GO                   ;21 IB = IB+1
        HELLO (MXDMR)
       MOVEI      ,3                    ;   ENTRY MXDMAR
GO:    MOVEM   A01,ACSAV+1              ;   IF (I.EQ.0.OR.J.EQ.0) RETURN
       HRRI    A01,ACSAV+2              ;   IB = 1
       HRLI    A01,2                    ;   DO 31 L=1,I
       BLT     A01,ACSAV+17             ;   DO 31 K=1,J
       SKIPG   A02,@3(A16)              ;31 IB = IB+1
       JRST       ,END                  ;   RETURN
       SKIPG   A03,@4(A16)              ;
       JRST       ,END                  ;   ENTRY MXDML
       MOVEI   A01,3                    ;
       IMUL    A01,                     ;   IF (I.EQ.0.OR.J.EQ.0) RETURN
       MOVEI      ,@0(A16)              ;   DO 41 L=1,I
       HRRM       ,COMTAB+0(A01)        ;   DO 41 K=1,J
       MOVEI      ,@1(A16)              ;   R(IB) = B(IB)*A(L)
       HRRM       ,COMTAB+1(A01)        ;41 IB = IB+1
       MOVEI      ,@2(A16)              ;   RETURN
       HRRM       ,COMTAB+2(A01)        ;
       HRLI       ,COMTAB(A01)          ;
       HRRI       ,LOOP                 ;   IF (I.EQ.0.OR.J.EQ.0) RETURN
       BLT        ,LOOP+2               ;   IB = 1
       MOVE       ,[XWD LOOPB+4,7]      ;   DO 51 L=1,I
       BLT        ,17                   ;   DO 51 K=1,J
       SOS        ,A02                  ;51 IB = IB + 1
       HRRM    A02,LOOP+7               ;   RETURN
       SOS        ,A03                  ;   END
       HRRM    A03,LOOP+4
       PAGE
       SETZM      ,A01                  ;CLEAR LOOP REGISTERS
       SETZM      ,A02
       SETZM      ,A03
LOOPB: JRST       ,4
       PHASE   4
LOOP:  XCT        ,COMTAB(A01)          ;EXECUTE THE ARITHMETIC
       XCT        ,COMTAB+1(A01)        ;INSIDE OF THE LOOPS
       XCT        ,COMTAB+2(A01)
       AOS        ,A03
       CAIGE   A02,                     ;END OF INNER LOOP
       AOJA    A02,LOOP
       SETZM      ,A02
       CAIGE   A01,                     ;END OF OUTER LOOP
       AOJA    A01,LOOP
       JRST       ,END
       DEPHASE
END:   HRLZI      ,ACSAV                ;RESTORE AC'S , RETURN
       BLT        ,17
        GOODBY
COMTAB:MOVE       ,(A01)                ;R(IB) = R(IB)+B(IB)*A(L)
       FMPR       ,(A03)
       FADRM      ,(A03)
       MOVE       ,(A03)                ;R(IB) = R(IB)+A(IB)*B(K)
       FMPR       ,(A02)
       FADRM      ,(A03)
       MOVE       ,(A01)                ;R(IB) = B(IB)*A(L)
       FMPR       ,(A03)
       MOVEM      ,(A03)
       MOVE       ,(A03)                ;R(IB) = A(IB)*B(K)
       FMPR       ,(A02)
       MOVEM      ,(A03)
ACSAV: BLOCK   20
       A01= 1
       A02= 2
       A03= 3
       A04= 4
       A05= 5
       A06= 6
       A07= 7
       A16=16
       PRGEND
#ifdef CERNLIB_TCGENR_MXDMAL
#undef CERNLIB_TCGENR_MXDMAL
#endif
