*
* $Id: floarg.s,v 1.1.1.1 1996/02/15 17:53:12 mclareni Exp $
*
* $Log: floarg.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:12  mclareni
* Kernlib
*
*
FLOARG   CSECT
*
* CERN PROGLIB# M250    FLOARG          .VERSION KERNIBM  2.06  830901
*
* R. MATTHEWS, CERN/DD, AUGUST 1983.
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
FLOARG   AMODE ANY
FLOARG   RMODE ANY
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
F0       EQU   0
F2       EQU   2
F4       EQU   4
F6       EQU   6
*
         PRINT NOGEN
*
*  P R O L O G U E
*
         USING FLOARG,R15
         B     SAVEREGS
         DC    AL1(7),CL7'FLOARG'    IDENTIFIER
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
         L     R2,0(0,R1)     R2 -> ARGUMENT
         LE    F0,0(0,R2)     F0 := ARGUMENT
         L     R3,0(0,R2)     R3 := ARGUMENT
         TM    0(R2),X'80'    IS THE SIGN BIT ON ?
         BO    NEGATIVE       YES, BRANCH
*
         TM    0(R2),X'7F'    IS THE ARGUMENT FLOATING POINT ?
         BNZ   EPILOGUE       PROBABLY, RETURN
*
         L     R2,=X'4E000000'  R2 := CHARACTERISTIC
         STM   R2,R3,FPNUM    STORE THE UNNORMALIZED NUMBER
         LD    F0,FPNUM       F0 := UNNORMALIZED NUMBER
         AD    F0,=D'0.0'     NORMALIZE IT
         B     EPILOGUE       RETURN
*
NEGATIVE TM    0(R2),X'7F'    IS THE ARGUMENT FLOATING POINT ?
         BNO   EPILOGUE       PROBABLY, RETURN
*
         LPR   R3,R3          R3 := ABS(ARGUMENT)
         L     R2,=X'CE000000'  R2 := CHARACTERISTIC
         STM   R2,R3,FPNUM    STORE THE UNNORMALIZED NUMBER
         LD    F0,FPNUM       F0 := UNNORMALIZED NUMBER
         AD    F0,=D'0.0'     NORMALIZE IT
*
*  E P I L O G U E
*
EPILOGUE EQU   *
         L     R13,4(0,R13)
         LM    R14,R12,12(R13)
         MVI   12(R13),X'FF'
         BR    R14
*
*  D A T A   A R E A S
*
FPNUM    DS    D
*
         LTORG
         END
#ifdef CERNLIB_TCGEN_FLOARG
#undef CERNLIB_TCGEN_FLOARG
#endif
