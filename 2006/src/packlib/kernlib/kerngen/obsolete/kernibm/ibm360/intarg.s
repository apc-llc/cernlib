*
* $Id: intarg.s,v 1.1.1.1 1996/02/15 17:53:13 mclareni Exp $
*
* $Log: intarg.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:13  mclareni
* Kernlib
*
*
INTARG   CSECT
*
* CERN PROGLIB# M250    INTARG          .VERSION KERNIBM  2.06  830901
*
* R. MATTHEWS, CERN/DD, AUGUST 1983.
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
INTARG   AMODE ANY
INTARG   RMODE ANY
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
         USING INTARG,R15
         B     SAVEREGS
         DC    AL1(7),CL7'INTARG'    IDENTIFIER
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
         L     R0,0(0,R2)     R0 := ARGUMENT
         TM    0(R2),X'80'    IS THE SIGN BIT ON ?
         BO    NEGATIVE       YES, BRANCH
*
         TM    0(R2),X'7F'    IS THE ARGUMENT INTEGER ?
         BZ    EPILOGUE       PROBABLY, RETURN
*
         SDR   F0,F0          CLEAR THE LOW-ORDER PART OF F0
         LE    F0,0(0,R2)     F0 := ARGUMENT
         AW    F0,=X'4E00000000000000'  SHIFT OUT THE FRACTIONAL PART
         STD   F0,FPNUM       STORE THE RESULT
         L     R0,FPNUM+4     R0 := INTEGER PART
         B     EPILOGUE       RETURN
*
NEGATIVE TM    0(R2),X'7F'    IS THE ARGUMENT INTEGER ?
         BO    EPILOGUE       PROBABLY, RETURN
*
         SDR   F0,F0          CLEAR THE LOW-ORDER PART OF F0
         LE    F0,0(0,R2)     F0 := ARGUMENT
         AW    F0,=X'4E00000000000000'  SHIFT OUT THE FRACTIONAL PART
         STD   F0,FPNUM       STORE THE RESULT
         L     R0,FPNUM+4     R0 := INTEGER PART
         LNR   R0,R0          NEGATE IT
*
*  E P I L O G U E
*
EPILOGUE EQU   *
         L     R13,4(0,R13)
         L     R14,12(R13)
         LM    R1,R12,24(R13)
         MVI   12(R13),X'FF'
         BR    R14
*
*  D A T A   A R E A S
*
FPNUM    DS    D
*
         LTORG
         END
#ifdef CERNLIB_TCGEN_INTARG
#undef CERNLIB_TCGEN_INTARG
#endif
