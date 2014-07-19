*
* $Id: mxdma.s,v 1.1.1.1 1996/02/15 17:53:46 mclareni Exp $
*
* $Log: mxdma.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:46  mclareni
* Kernlib
*
*
        TITLE   MXDMA
        SUBTTL  P. SCHMITZ
        SEARCH HELP
        HELLO (MXDMA)
       HRRZI      ,ACSAV
       BLT        ,ACSAV+4
       MOVE       ,@4(A16)              ;   SUBROUTINE MXDMA (A,F,D,B,I)
       SKIPG                            ;   DIMENSION A(400),B(400),D(20
        GOODBY
       MOVEI   A10,1                    ;   IF (I.EQ.1) GO TO 12
       SETZM      ,A11                  ;   DO 11 K=2,I
       SETZM      ,A12                  ;   B(J) = A(J)+F*D(K-1)
       SOS                              ;   DO 11 L=1,I
       JUMPE      ,LOOPE                ;   B(J) = A(J)
       MOVEI   A13,@0(A16)              ;12 B(J) = A(J)+F*D(I)
       HRRM    A13,LOOP+2               ;   RETURN
       HRRM    A13,LOOP1                ;   END
       HRRM    A13,LOOPE+4
       MOVEI   A13,@2(A16)
       SOS        ,A13
       HRRM    A13,LOOP
       MOVEI   A13,@3(A16)
       HRRM    A13,LOOP+3
       HRRM    A13,LOOP1+1
       HRRM    A13,LOOPE+5
LOOP:  MOVE    A13,(A10)                ;B(J) = A(J)+F*D(K-1)
       FMPR    A13,@1(A16)
       FADR    A13,(A12)
       MOVEM   A13,(A12)
       AOS        ,A12                  ;J = J+1
       SETZM      ,A11
LOOP1: MOVE    A13,(A12)                ;B(J) = A(J)
       MOVEM   A13,(A12)
       AOS        ,A12                  ;J = J+1
       CAMGE   A11,                     ;END OF INNER LOOP
       AOJA    A11,LOOP1
       CAMGE   A10,                     ;END OF OUTER LOOP
       AOJA    A10,LOOP
LOOPE: MOVEI   A10,@2(A16)              ;B(J) = A(J)+F*D(I)
       ADD     A10,
       MOVE    A13,(A10)
       FMPR    A13,@1(A16)
       FADR    A13,(A12)
       MOVEM   A13,(A12)
       HRLZI      ,ACSAV
       BLT        ,4
        GOODBY
ACSAV: BLOCK   5
       A10= 1
       A11= 2
       A12= 3
       A13= 4
       A16=16
       PRGEND
#ifdef CERNLIB_TCGENR_MXDMA
#undef CERNLIB_TCGENR_MXDMA
#endif
