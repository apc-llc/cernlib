*
* $Id: mxuty.s,v 1.1.1.1 1996/02/15 17:53:16 mclareni Exp $
*
* $Log: mxuty.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:16  mclareni
* Kernlib
*
*
MXUTY    CSECT
*
* CERN PROGLIB# F110    MXUTY           .VERSION KERNIBM  0.1   720518
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
MXUTY    AMODE ANY
MXUTY    RMODE ANY
#endif
         ENTRY MXUTY                   CALL MXUTY(A,I)
         BC    15,12(15)
         DC    X'07'
         DC    CL7'MXUTY  '
         STM   14,12,12(13)
         LR    2,15
         USING MXUTY,2
         ST    13,SA+4
         LR    12,13
         LA    13,SA
         ST    13,8(12)
         L     5,4(1)                  I
         L     5,0(5)
         LTR   5,5
         BC    9,RETURN                IF I=0 RETURN
         LA    6,0(0)                  J = 1 IN REG. 6
         L     7,0(1)                  A
         LE    2,EINS
         S     5,FIXEINS               I - 1 IN REG. 5
         BC    9,SKIP                  IF I=1 BRANCH TO SKIP
         LA    3,1(0)                  K = 2 IN REG. 3
         LE    4,NULL
LOOPK    STE   2,0(6,7)                A(J) = 1.
         LA    6,4(6)
         LA    8,0(0)                  L = 1  IN REG. 8
LOOPL    STE   4,0(6,7)                A(J) = 0.
         LA    6,4(6)
         LA    8,1(8)                  INCREASE L
         CR    8,5
         BC    12,LOOPL
         LA    3,1(3)                  INCREASE K
         CR    3,5                     TEST K
         BC    12,LOOPK
SKIP     STE   2,0(6,7)                A(J) = 1.
RETURN   L     13,SA+4
         LM    14,12,12(13)
         BCR   15,14
SA       DS    18F
NULL     DC    E'0.'
EINS     DC    E'1.'
FIXEINS  DC    F'1'
         END
#ifdef CERNLIB_TCGEN_MXUTY
#undef CERNLIB_TCGEN_MXUTY
#endif
