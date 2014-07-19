*
* $Id: pkbyt.s,v 1.1.1.1 1996/02/15 17:53:16 mclareni Exp $
*
* $Log: pkbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:16  mclareni
* Kernlib
*
*
PKBYT    CSECT
*
* CERN PROGLIB# M422    PKBYT           .VERSION KERNIBM  2.05  830421
* ORIG.  FEB 83,  R.MATTHEWS, CERN/DD
*
*     CALL PKBYT (IB,X,JX,N,MPACK)
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
PKBYT    AMODE ANY
PKBYT    RMODE ANY
#endif
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*
         PRINT NOGEN
*
*  P R O L O G U E
*
         USING PKBYT,R15
         B     SAVEREGS
         DC    AL1(7),CL7'PKBYT'     IDENTIFIER
         DC    C'1.00'               VERSION.MODIFICATION LEVEL
SAVEREGS STM   R14,R12,12(R13)
         LR    R4,R13
         CNOP  0,4
         BAL   R13,AROUNDSA
SAVEAREA DC    A(EPILOGUE),17F'0'
         DROP  R15
         USING SAVEAREA,R13
AROUNDSA ST    R13,8(0,R4)
         ST    R4,4(0,R13)
         XC    8(4,R13),8(R13)
*
         LM    R2,R6,0(R1)    R2 -> IB;  R3 -> X;  R4 -> JX;
*                             R5 -> N;   R6 -> MPACK
         L     R5,0(0,R5)     R5 := N
         LTR   R5,R5          N .LE. 0 ?
         BNP   EPILOGUE       YES, BRANCH
*
         L     R7,4(0,R6)     R7 := INWORD
         L     R6,0(0,R6)     R6 := NBITS
         LTR   R6,R6          NBITS .GT.0 ?
         BP    L1             YES, BRANCH
*
         LA    R6,1           R6 := 1   (NBITS)
         LA    R7,32          R7 := 32  (INWORD)
*
L1       SR    R0,R0          R0 := 0
         L     R1,0(0,R4)     R1 := JX
         BCTR  R1,0
         DR    R0,R7
         SLL   R1,2
         LA    R3,0(R1,R3)    R3 -> FIRST TARGET WORD
*
         LR    R1,R0
         MR    R0,R6
         LR    R8,R1          R8 -> FIRST TARGET 'BYTE' WITHIN WORD
*
         LR    R1,R7
         BCTR  R1,0
         MR    R0,R6
         LR    R7,R1          R7 -> LAST TARGET 'BYTE' WITHIN WORD
*
         BCTR  R5,0
         SLL   R5,2
         LA    R5,0(R5,R2)    R5 -> LAST INPUT WORD
         LA    R4,4           R4 := 4
*
         L     R9,=X'FFFFFFFF'
         LR    R11,R9
         SR    R10,R10
         SLDL  R10,0(R6)      R10 := MASK TO ISOLATE INPUT 'BYTE'
         LR    R12,R11        R11 & R12 := MASK FOR TARGET 'BYTE'
         XR    R11,R9         INVERT BITS
         SLL   R11,0(R8)      SHIFT TO INITIAL POSITION
         XR    R11,R9         INVERT BITS
*
         L     R0,0(0,R3)     R0 := FIRST TARGET WORD
         B     L4
*
L2       ST    R0,0(0,R3)     STORE THE PACKED WORD
         AR    R3,R4          R3 -> NEXT TARGET WORD
         L     R0,0(0,R3)     R0 := NEXT TARGET WORD
         LNR   R8,R6          PRIME R8
         LR    R11,R12        INITIALISE MASK FOR TARGET 'BYTES'
*
L3       BXH   R8,R6,L2
L4       L     R1,0(0,R2)     R1 := NEXT INPUT WORD
         NR    R1,R10         R1 := NEXT INPUT 'BYTE'
         SLL   R1,0(R8)       SHIFT IT TO THE CORRECT 'BYTE' LOCATION
         NR    R0,R11         CREATE A HOLE IN THE TARGET WORD
         XR    R11,R9         FLIP BITS
         SLL   R11,0(R6)      SHIFT FOR NEXT 'BYTE'
         XR    R11,R9         FLIP BITS
         OR    R0,R1          PACK IT INTO THE TARGET WORD
         BXLE  R2,R4,L3       R2 -> NEXT INPUT WORD
*
         ST    R0,0(0,R3)     STORE THE FINAL PACKED WORD
*
*  E P I L O G U E
*
EPILOGUE EQU   *
         L     R13,4(0,R13)
         LM    R14,R12,12(R13)
         MVI   12(R13),X'FF'
         BR    R14
*
         LTORG
*
         END
#ifdef CERNLIB_TCGEN_PKBYT
#undef CERNLIB_TCGEN_PKBYT
#endif
