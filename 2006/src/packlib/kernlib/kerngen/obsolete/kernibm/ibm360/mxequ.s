*
* $Id: mxequ.s,v 1.1.1.1 1996/02/15 17:53:15 mclareni Exp $
*
* $Log: mxequ.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:15  mclareni
* Kernlib
*
*
MXEQU    CSECT
*
* CERN PROGLIB# F109    MXEQU           .VERSION KERNIBM  0.1   720518
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
MXEQU    AMODE ANY
MXEQU    RMODE ANY
#endif
         ENTRY MXEQU                   CALL MXEQU(A,B,I,J)
         ENTRY MXEQU1                  CALL MXEQU1(A,B,I,J)
         BC    15,12(15)
         DC    X'07'
         DC    CL7'MXEQU  '
         STM   14,12,12(13)
         LR    2,15
         USING MXEQU,2
         L     5,8(1)                  ADR(I)
         L     5,0(5)                  I IN REG. 5
         LTR   5,5
         BC    12,RETURN1              IF I=0 RETURN
         ST    13,SA+4                 STORE SA
         LR    12,13
         LA    13,SA
         ST    13,8(12)
         ST    1,REG1
         LA    15,4(0)                 4 IN REG. 15
         L     3,0(1)                  ADR(A) IN REG. 3
         L     4,4(1)                  ADR(B) IN REG. 4
         B     ZEHN
         DS    0F
MXEQU1   BC    15,16(15)
         DC    X'07'
         DC    CL7'MXEQU1 '
         DC    A(MXEQU)
         STM   14,12,12(13)
         L     2,12(15)
         USING MXEQU,2
         L     5,8(1)                  ADR(I)
         L     5,0(5)                  I IN REG. 5
         LTR   5,5
         BC    12,RETURN1              IF I=0 RETURN
         ST    13,SA+4                 STORE SA
         LR    12,13
         LA    13,SA
         ST    13,8(12)
         ST    1,REG1
         LA    15,4(0)                 4 IN REG. 15
         L     3,0(1)                  ADR(A) IN REG. 3
         L     4,4(1)                  ADR(B) IN REG. 4
         B     TESTJ
ZEHN     LA    8,1(0)                  L = 1 IN REG. 8
LOOPL1   LR    1,8                     COMPUTE ADR OF A(L,L)
         MR    0,5
         SR    1,5
         AR    1,8
         SLL   1,2(0)
         SR    1,15                    ADR OF A(L,L) IN REG. 1
         SDR   0,0
         LE    0,0(1,3)                DIAG=A(L,L) IN FL.REG. 0
         LA    7,1(0)                  M = 1 IN REG. 7
LOOPM1   CR    7,8
         BC    8,ZWOELF                IF M = L GO TO 12
         LR    1,7                     COMPUTE ADR OF A(L,M)
         MR    0,5
         SR    1,5
         AR    1,8
         SLL   1,2(0)
         SR    1,15                    ADR OF A(L,M) IN REG. 1
         SDR   2,2
         SDR   4,4
         LE    4,0(1,3)                A(L,M) IN FL.REG. 4
         SDR   2,4                     -A(L,M) IN FL.REG. 2
         DDR   2,0                     -A(L,M)/DIAG IN FL.REG. 2
         STE   2,0(1,3)                A(L,M)=-A(L,M)/DIAG
ZWOELF   LA    7,1(7)                  INCREASE M
         CR    7,5
         BC    12,LOOPM1               IS LOOPM1 FINISHED &
         LR    9,8
         LA    9,1(9)                  LPL1=L+1 IN REG. 9
         CR    9,5
         BC    2,TESTJ                 IF LPL1 > I GO TO TESTJ
LOOPLPL1 LA    7,1(0)                  M = 1 IN REG. 7
LOOPM2   CR    8,7
         BC    8,EINS5                 IF M=L GO TO 15
         LR    1,7                     COMPUTE ADR. OF A(LPL1,M)
         MR    0,5
         SR    1,5
         AR    1,9
         SLL   1,2(0)
         SR    1,15
         LR    10,1                    ADR. OF A(L1,M) IN REG. 10
         LR    1,8                     COMPUTE ADR. OF A(L1,L)
         MR    0,5
         SR    1,5
         AR    1,9
         SLL   1,2(0)
         SR    1,15
         LR    11,1                    ADR. OF A(L1,L) IN REG. 11
         LR    1,7                     COMPUTE ADR. OF A(L,M)
         MR    0,5
         SR    1,5
         AR    1,8
         SLL   1,2(0)
         SR    1,15                    ADR. OF A(L,M) IN REG. 1
         SDR   0,0
         SDR   2,2
         SDR   4,4
         LE    0,0(1,3)                A(L,M) IN FL.REG. 0
         LE    2,0(11,3)               A(L1,L) IN FL.REG. 2
         MDR   0,2                     A(L,M)*A(L1,L)
         LE    4,0(10,3)               A(L1,M) IN FL.REG. 4
         ADR   0,4                     A(L1,M) + A(L,M)*A(L1,L)
         STE   0,0(10,3)               STORE A(L1,M)
EINS5    LA    7,1(7)                  INCREASE M
         CR    7,5
         BC    12,LOOPM2               IS LOOPM2 FINISHED
         LA    9,1(9)                  INCREASE LPL1
         CR    9,5
         BC    12,LOOPLPL1             IS LOOPLPL1 FINISHED
         LA    8,1(8)                  INCREASE L
         CR    8,5
         BC    12,LOOPL1               IS LOOPL1 FINISHED &
TESTJ    L     6,REG1
         L     6,12(6)
         L     6,0(6)                  J IN REG. 6
         LTR   6,6
         BC    12,RETURN               IF J=0 RETURN
         LA    8,1(0)                  L = 1 IN REG. 8
LOOPL2   LA    7,1(0)                  M = 1 IN REG. 7
LOOPM3   LA    9,1(0)                  N = 1 IN REG. 9
LOOPN    CR    8,9
         BC    8,ZWEI5                 IF L=N GO TO 25
         LR    1,9                     COMPUTE ADR. OF B(M,N)
         MR    0,6
         SR    1,6
         AR    1,7
         SLL   1,2(0)
         SR    1,15
         LR    10,1                    ADR. OF B(M,N) IN REG. 10
         LR    1,8                     COMPUTE ADR. OF B(M,L)
         MR    0,6
         SR    1,6
         AR    1,7
         SLL   1,2(0)
         SR    1,15
         LR    11,1                    ADR. OF B(M,L) IN REG. 11
         LR    1,9                     COMPUTE ADR. OF A(L,N)
         MR    0,5
         SR    1,5
         AR    1,8
         SLL   1,2(0)
         SR    1,15                    ADR. OF A(L,N) IN REG. 1
         SDR   0,0
         SDR   2,2
         SDR   4,4
         LE    0,0(1,3)                A(L,N) IN FL.REG. 0
         LE    2,0(11,4)               B(M,L) IN FL.REG. 2
         MDR   0,2                     A(L,N)*B(M,L)
         LE    4,0(10,4)               B(M,N) IN FL.REG. 4
         ADR   0,4                     A(L,N)*B(M,L)+B(M,N)
         STE   0,0(10,4)               STORE NEW B(M,N)
ZWEI5    LA    9,1(9)                  INCREASE N
         CR    9,5
         BC    12,LOOPN                IS LOOP N FINISHED &
         LA    7,1(7)                  INCREASE M
         CR    7,6
         BC    12,LOOPM3               IS LOOP M FINISHED
         LA    8,1(8)                  INCREASE L
         CR    8,5
         BC    12,LOOPL2
         LA    7,1(0)                  M = 1 IN REG. 7
LOOPM4   LA    9,1(0)                  N = 1 IN REG. 9
LOOPN1   LR    1,9                     COMPUTE ADR. OF B(M,N)
         MR    0,6
         SR    1,6
         AR    1,7
         SLL   1,2(0)
         SR    1,15
         LR    10,1                    ADR. OF B(M,N) IN REG. 10
         LR    1,9                     COMPUTE ADR. OF A(N,N)
         MR    0,5
         SR    1,5
         AR    1,9
         SLL   1,2(0)
         SR    1,15                    ADR. OF A(N,N) IN REG. 1
         SDR   0,0
         SDR   2,2
         LE    0,0(10,4)               B(M,N) IN FL.REG. 0
         LE    2,0(1,3)                A(N,N) IN FL.REG. 2
         DDR   0,2                     B7
         STE   0,0(10,4)               STORE NEW B(M,N)
         LA    9,1(9)                  INCREASE N
         CR    9,5
         BC    12,LOOPN1               IS LOOP N FINISHED &
         LA    7,1(7)                  INCREASE M
         CR    7,6
         BC    12,LOOPM4               IS LOOP M FINISHED
RETURN   L     13,SA+4
RETURN1  LM    14,12,12(13)
         BCR   15,14
REG1     DS    1F
SA       DS    18F
         END
#ifdef CERNLIB_TCGEN_MXEQU
#undef CERNLIB_TCGEN_MXEQU
#endif
