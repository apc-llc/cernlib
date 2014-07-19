*
* $Id: matin1.s,v 1.1.1.1 1996/02/15 17:53:15 mclareni Exp $
*
* $Log: matin1.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:15  mclareni
* Kernlib
*
*
MATIN1   CSECT
*
* CERN PROGLIB# F100    MATIN1          .VERSION KERNIBM  1.09  820119
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
MATIN1   AMODE ANY
MATIN1   RMODE ANY
#endif
         ENTRY MATIN1               .CALL MATIN1(A,DIM,N,DIM2,N2
*                                      INDEX,NERROR,DETERM)
         BC    15,12(15)
         DC    X'07'
         DC    CL7'MATIN1 '
         STM   14,12,12(13)
         LR    2,15
         USING MATIN1,2
         ST    13,SA+4
         LR    12,13
         LA    13,SA
         ST    13,8(12)
         LD    6,FLOATONE              DETER = 1.  IN FL.REG. 6
         L     15,8(1)
         L     15,0(15)
         SLL   15,2(0)
         S     15,FIXVIER              4*(N-1)  IN REG. 15
         L     3,0(1)                  ADR. OF A IN REG. 3
         L     4,20(1)                 ADR OF INDEX  IN REG. 4
         L     5,16(1)
         L     5,0(5)
         SLL   5,2(0)
         AR    5,15                    EMAT = 4(N-1)+4*N2  IN REG.5
         L     6,4(1)
         L     6,0(6)
         SLL   6,2(0)                  4*DIM  IN REG. 6
         LNR   7,6                     PIVCOL=-DIM  IN REG. 7
         LA    8,0(0)                  MAIN  IN REG. 8
LOOPM    SDR   4,4                     PIVOT = 0. IN FL.REG. 4
         AR    7,6                     PIVCOL = PIVCOL + DIM  IN REG. 7
         LR    9,7
         AR    9,8                     PIVCL1=PIVCOL+MAIN  IN REG. 9
         LR    10,7
         AR    10,15                   PIVCL2=PIVCOL+NMIN1  IN REG. 10
         LR    11,9                    I = PIVCL1  IN REG. 11
LOOPI1   LPDR  0,4                     ABS(PIVOT)  IN FL.REG. 0
         SDR   2,2                     CLEAR FL.REG. 2
         LE    2,0(11,3)               A(I)  IN FL.REG. 2
         LPDR  2,2                     ABS( A(I) )  IN FL.REG. 2
         CDR   2,0
         BC    12,ZWEI                 IF ABS(A(I)) LE ABS(PIVOT) GOTO2
         SDR   4,4                     CLEAR FL.REG. 4
         LE    4,0(11,3)               PIVOT = A(I)  IN FL.REG. 4
         LR    0,11                    LPIV = I  IN REG. 0
ZWEI     LA    11,4(11)                INCREASE I
         CR    11,10
         BC    12,LOOPI1
         LTDR  4,4
         BC    8,EINS5                 IF PIVOT = 0  GO TO 15
         LR    12,0
         SR    12,7                    ICOL=LPIV-PIVCOL  IN REG. 12
         ST    12,0(8,4)               INDEX(MAIN) = ICOL
         CR    12,8
         BC    12,SECHS                IF ICOL LE MAIN  GO TO 6
         SDR   2,2                     CLEAR FL.REG. 2
         SDR   2,6
         LDR   6,2                     DETER = -DETER  IN FL.REG. 6
         SR    12,6                    ICOL=ICOL-DIM  IN REG. 12
         LR    14,8
         SR    14,6                    I3 = MAIN-DIM  IN REG. 14
         LA    11,0(0)                 I = 0 IN REG. 11
LOOPI2   AR    12,6                    ICOL = ICOL+DIM  IN REG. 12
         AR    14,6                    I3 = I3+DIM  IN REG. 14
         LE    2,0(14,3)               SWAP = A(I3)  IN FL.REG. 2
         LE    0,0(12,3)
         STE   0,0(14,3)               A(I3) = A(ICOL)
         STE   2,0(12,3)               A(ICOL) = SWAP
         LA    11,4(11)                INCREASE I
         CR    11,5
         BC    12,LOOPI2
SECHS    MDR   6,4                     DETER = DETER*PIVOT
         LD    0,FLOATONE              1.  IN FL.REG. 0
         DDR   0,4                     1./PIVOT  IN FL.REG. 0
         LDR   4,0                     PIVOT = 1./PIVOT  IN FL.REG. 4
         LR    14,7
         AR    14,15                   I3 = PIVCOL+NMIN1  IN REG. 14
         LR    11,7                    I = PIVCOL  IN REG. 7
LOOPI3   SDR   0,0                     CLEAR FL.REG. 0
         SE    0,0(11,3)               -A(I) IN FL.REG. 0
         MDR   0,4                     -A(I)*PIVOT  IN FL.REG. 0
         STE   0,0(11,3)               A(I) = -A(I)*PIVOT
         LA    11,4(11)                INCREASE I
         CR    11,14
         BC    12,LOOPI3
         STE   4,0(9,3)                A(PIVCL1) = PIVOT
         LR    10,8
         SR    10,6                    I1 = MAIN-DIM  IN REG. 10
         LNR   12,6                    ICOL = -DIM  IN REG. 12
         LA    11,0(0)                 I = 0  IN REG. 11
LOOPI4   AR    12,6                    ICOL = ICOL+DIM
         AR    10,6                    I1 = I1+DIM  IN REG. 10
         CR    11,8
         BC    8,ZEHN                  IF I .EQ. MAIN  GO TO 10
         LR    13,12
         AR    13,15                   JCOL = ICOL+NMIN1  IN REG. 13
         SDR   2,2                     CLEAR FL.REG. 2
         LE    2,0(10,3)               SWAP = A(I1)  IN FL.REG. 2
         LR    14,7
         S     14,FIXVIER              I3 = PIVCOL-1  IN REG. 14
         LR    9,12                    I2 = ICOL  IN REG. 9
LOOPI21  LA    14,4(14)                I3 = I3+1  IN REG. 14
         SDR   0,0                     CLEAR FL.REG. 0
         LE    0,0(14,3)               A(I3)  IN FL.REG. 0
         MDR   0,2                     SWAP*A(I3)  IN FL.REG. 0
         AE    0,0(9,3)                A(I2)+SWAP*A(I3)  IN FL.REG. 0
         STE   0,0(9,3)                A(I2)=A(I2)+SWAP*A(I3)
         LA    9,4(9)                  INCREASE I3
         CR    9,13
         BC    12,LOOPI21
         MDR   2,4
         STE   2,0(10,3)               A(I1) = SWAP*PIVOT
ZEHN     LA    11,4(11)                INCREASE I
         CR    11,5
         BC    12,LOOPI4
         LA    8,4(8)                  INCREASE MAIN
         CR    8,15
         BC    12,LOOPM
         LA    14,0(0)                 I1 = 0  IN REG. 14
LOOPI11  LR    9,15
         SR    9,14                    MAIN = NMIN1 - I1  IN REG. 9
         L     11,0(9,4)               LPIV=INDEX(MAIN)  IN REG. 11
         CR    11,9
         BC    8,EINS4                 IF LPIV .EQ. MAIN GO TO 14
         MR    10,6                    ICOL=LPIV*DIM  IN REG. 11
         SRL   11,2(0)
         LR    10,11
         AR    10,15                   JCOL=ICOL+NMIN1  IN REG. 10
         MR    8,6
         SRL   9,2(0)
         SR    9,11                    PIVCOL=MAIN*DIM-ICOL  IN REG. 9
         LR    12,11                   I2 = ICOL  IN REG. 12
LOOPI22  LR    13,12
         AR    13,9                    I3 = I2+PIVCOL  IN REG. 13
         LE    2,0(12,3)               SWAP = A(I2)  IN FL.REG. 2
         LE    0,0(13,3)
         STE   0,0(12,3)               A(I2) = A(I3)
         STE   2,0(13,3)               A(I3) = SWAP
         LA    12,4(12)                INCREASE I2
         CR    12,10
         BC    12,LOOPI22
EINS4    LA    14,4(14)                INCREASE I1
         CR    14,15
         BC    12,LOOPI11
         L     3,28(1)                 ADR. OF DETERM  IN REG. 3
         STE   6,0(3)                  DETERM = DETER
         L     3,24(1)                 ADR. OF NERROR  IN REG. 3
         LA    0,0(0)                  0 IN REG. 0
         ST    0,0(3)                  NERROR = 0
         B     RETURN
EINS5     EQU    *
          L      3,24(1)           ADR. OF NERROR IN REG. 3
          ST     8,0(3)            NERROR=MAIN
          L      3,28(1)           ADR. OF DETERM IN REG. 3
          XC     0(4,3),0(3)       DETERM=0.
RETURN   L     13,SA+4
         LM    14,12,12(13)
         BCR   15,14
FLOATONE DC    D'1.'
FIXVIER  DC    F'4'
SA       DS    18F
         END
#ifdef CERNLIB_TCGEN_MATIN1
#undef CERNLIB_TCGEN_MATIN1
#endif
