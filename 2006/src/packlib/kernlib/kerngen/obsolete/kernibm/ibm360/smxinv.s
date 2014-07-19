*
* $Id: smxinv.s,v 1.1.1.1 1996/02/15 17:53:16 mclareni Exp $
*
* $Log: smxinv.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:16  mclareni
* Kernlib
*
*
SMXINV   CSECT
*
* CERN PROGLIB# F107    SMXINV          .VERSION KERNIBM  0.1   720518
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
SMXINV   AMODE ANY
SMXINV   RMODE ANY
#endif
         ENTRY SMXINV                  CALL SMXINV(A,N,IFAIL)
         STM   14,12,12(13)
         LR    2,15
         USING SMXINV,2
         ST    13,SA+4
         LR    12,13
         LA    13,SA
         ST    13,8(12)
         LA    0,0(0)                  0 IN REG. 0
         L     15,8(1)                 ADR. OF IFAIL IN REG. 15
         ST    0,0(15)                 IFAIL = 0
         L     3,4(1)                  ADR. OF N IN REG. 3
         L     3,0(3)                  N IN REG. 3
         SLL   3,1(0)                  2*N IN REG. 3
         S     3,FIXEINS               2*N-1 IN REG. 3
         STC   3,INDEX1+1              DEFINE LENGTH OF INDEX
         A     3,FIXEINS               2*N IN REG. 3
         SLL   3,1(0)                  4*N IN REG. 3
         MVI   INDEX,X'01'
INDEX1   MVC   INDEX+1,INDEX           SET INDEX = 1
         L     4,0(1)                  ADR. OF A IN REG. 4
         LA    5,INDEX                 ADR. OF INDEX IN REG. 5
         LA    6,RI                    ADR. OF RI IN REG. 6
         SDR   4,4
         LE    4,TOL                   TOL IN FL.REG. 4
         LR    14,3
         LA    14,4(14)                NP1 = N + 1  IN REG. 14
         LA    7,0(0)                  I = 0 IN REG. 7
LOOPI1   SDR   0,0                     PIVOT = 0. IN FL.REG. 0
         LA    8,0(0)                  JJ = 0  IN REG. 8
         LA    9,0(0)                  ZAEHLER J  IN REG. 9
         LA    10,0(0)                 INDEX J  IN REG. 10
LOOPJ1   CH    0,0(10,5)
         BC    8,EINS9                 IF INDEX(J) = 0  GO TO 19
         SDR   2,2
         LE    2,0(8,4)                A(JJ) IN FL.REG. 2
         LPDR  2,2                     ELM=ABS( A(JJ) )  IN FL.REG. 2
         CDR   2,0
         BC    12,EINS9                IF ELM .LE. PIVOT  GO TO 19
         LDR   0,2                     PIVOT = ELM  IN FL.REG. 2
         LR    11,10                   INDEX K = INDEX J  IN REG. 11
         LR    12,8                    KK = JJ  IN REG. 12
EINS9    AR    8,14                    JJ = JJ + NP1 IN REG. 8
         LA    10,2(10)                INCREASE INDEX J
         LA    9,4(9)                  INCREASE ZAEHLER J
         CR    9,3
         BC    4,LOOPJ1
         SDR   6,6
         LE    6,0(4)                  A(1) IN FL.REG. 6
         LPDR  6,6                     ABS( A(1) ) IN FL.REG. 6
         DDR   0,6                     PIVOT/ABS( A(1) )  IN FL.REG. 0
         CDR   0,4
         BC    4,EINS00                IF PIVOT/ABS(A(1)) [ TOL GOTO100
         STH   0,0(11,5)               INDEX(K) = 0
         SDR   0,0
         SE    0,0(12,4)               PIVOT = -A(KK)  IN FL.REG. 0
         SLL   11,1(0)                 K NO MORE INDEX FOR INDEX
         LR    8,11                    KJ = K IN REG. 8
         LR    9,3                     NP = N  IN REG. 9
         LA    10,0(0)                 J = 0  IN REG. 10
         LA    15,0(0)                 INDEX J  IN REG. 15
LOOPJ2   CR    11,10
         BC    6,DREI4                 IF K .NE. J  GO TO 34
         LD    6,FLOATONE
         DDR   6,0                     1./PIVOT IN FL.REG. 6
         STE   6,0(8,4)                A(KJ) = 1./PIVOT
         SDR   6,6
         STD   6,0(15,6)               RI(J) = 0.
         LA    9,4(0)                  NP = 1  IN REG. 9
         B     SIEBEN0
DREI4    SDR   2,2
         SE    2,0(8,4)                ELM = -A(KJ)  IN FL.REG. 2
         LDR   6,2
         DDR   6,0                     ELM/PIVOT  IN FL.REG. 6
         STD   6,0(15,6)               RI(J) = ELM/PIVOT
         SDR   6,6
         CDR   2,6
         BC    8,FUENF0                IF ELM=0.  GO TO 50
         LR    13,10                   JL = J  IN REG. 13
         LA    12,0(0)                 L = 0  IN REG. 12
LOOPL    LD    6,0(12,6)               RI(L) IN FL.REG. 6
         MDR   6,2                     RI(L)*ELM  IN FL.REG. 6
         AE    6,0(13,4)               A(JL)+RI(L)*ELM  IN FL.REG. 6
         STE   6,0(13,4)               A(JL)=A(JL)+ELM*RI(L)
         AR    13,3                    JL = JL + N  IN REG. 13
         LA    12,8(12)                INCREASE L
         CR    12,15
         BC    12,LOOPL
FUENF0   LD    6,0(15,6)               RI(J) IN FL.REG. 6
         STE   6,0(8,4)                A(KJ) = RI(J)
SIEBEN0  AR    8,9                     KJ = KJ + NP   IN REG. 8
         LA    15,8(15)                INCREASE INDEX J
         LA    10,4(10)                INCREASE ZAEHLER J
         CR    10,3
         BC    4,LOOPJ2
         LA    7,4(7)                  INCREASE I
         CR    7,3
         BC    4,LOOPI1
         LA    7,0(0)                  IJ0 = 0  IN REG. 7
         LA    8,0(0)                  JI0 = 0  IN REG. 8
         LA    9,0(0)                  I = 1  IN REG. 9
LOOPI2   LR    10,7                    IJ = IJ0  IN REG. 10
         LR    11,8                    JI = JI0  IN REG. 11
         LA    12,0(0)                 J = 1  IN REG. 12
LOOPJ3   SER   6,6
         SE    6,0(10,4)               -A(IJ)  IN FL.REG. 6
         STE   6,0(10,4)               A(IJ) = -A(IJ)
         STE   6,0(11,4)               A(JI) = A(IJ)
         AR    10,3                    IJ = IJ + N  IN REG. 10
         LA    11,4(11)                JI = JI + 4
         LA    12,4(12)                INCREASE J
         CR    12,9
         BC    12,LOOPJ3
         LA    7,4(7)                  IJ0 = IJ0 + 4
         AR    8,3                     JI0 = JI0 + N  IN REG. 8
         LA    9,4(9)                  INCREASE I
         CR    9,3
         BC    4,LOOPI2
         B     RETURN
EINS00   LA    3,1(0)
         L     15,8(1)
         ST    3,0(15)                 IFAIL = 1
RETURN   L     13,SA+4
         LM    14,12,12(13)
         BCR   15,14
RI       DS    100D
FLOATONE DC    D'1.'
SA       DS    18F
FIXEINS  DC    F'1'
TOL      DC    E'1.E-12'
INDEX    DS    100H
         END
#ifdef CERNLIB_TCGEN_SMXINV
#undef CERNLIB_TCGEN_SMXINV
#endif
