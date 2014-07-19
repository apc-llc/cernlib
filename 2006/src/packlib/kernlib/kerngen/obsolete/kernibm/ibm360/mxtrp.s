*
* $Id: mxtrp.s,v 1.1.1.1 1996/02/15 17:53:15 mclareni Exp $
*
* $Log: mxtrp.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:15  mclareni
* Kernlib
*
*
MXTRP    CSECT
*
* CERN PROGLIB# F110    MXTRP           .VERSION KERNIBM  0.1   720518
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
MXTRP    AMODE ANY
MXTRP    RMODE ANY
#endif
         ENTRY MXTRP                   CALL MXTRP(A,B,I,J)
         BC    15,12(15)
         DC    X'07'
         DC    CL7'MXTRP  '
         STM   14,12,12(13)
         LR    2,15
         USING MXTRP,2
         ST    13,SA+4
         LR    12,13
         LA    13,SA
         ST    13,8(12)
         L     5,8(1)                  I
         L     5,0(5)                  I IN REG. 5
         LTR   5,5
         BC    9,RETURN                IF I=0 RETURN
         L     7,12(1)                 J
         L     7,0(7)
         LTR   7,7
         BC    9,RETURN                IF J=0 RETURN
         S     7,EINS
         SLL   7,2(0)                  4*(J-1) IN REG. 7
         LA    4,1(0)                  INKREMENT 1 IN REG. 4
         LA    6,4(0)                  INKREMENT 4 IN REG. 6
         L     3,0(1)                  A
         L     8,4(1)                  B
         LA    9,0(0)                  IB = 1 IN REG. 9
         LA    10,0(0)                 K = 1 IN REG. 10
LOOPK    LR    11,10                   IA = K IN REG. 11
         LA    12,1(0)                 L = 1 IN REG. 12
LOOPL    LE    2,0(11,3)
         STE   2,0(9,8)                B(IB) = A(IA)
         AR    11,7
         AR    11,6                    IA = IA + J
         AR    9,6                     IB = IB + 1
         BXLE  12,4,LOOPL
         BXLE  10,6,LOOPK
RETURN   L     13,SA+4
         LM    14,12,12(13)
         BCR   15,14
SA       DS    18F
EINS     DC    F'1'
         END
#ifdef CERNLIB_TCGEN_MXTRP
#undef CERNLIB_TCGEN_MXTRP
#endif
