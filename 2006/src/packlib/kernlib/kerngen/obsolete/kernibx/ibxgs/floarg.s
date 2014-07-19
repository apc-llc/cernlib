*
* $Id: floarg.s,v 1.1.1.1 1996/02/15 17:54:36 mclareni Exp $
*
* $Log: floarg.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:36  mclareni
* Kernlib
*
*
FLOARG   CSECT
*
* CERN PROGLIB# M250    FLOARG          .VERSION KERNIBX  1.01  900523
*
* R. MATTHEWS, CERN/DD, AUGUST 1983.
* Modified for AIX, Roger Howard, January 1990
*
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
*  P R O L O G U E
*
         USING *,R15
         ENTRY _floarg_
_floarg_ STM   R2,R1,16(R13)
         LR    R4,R13
         USING STACK,R13
         S     R13,=A(STEND-STACK)
         ST    R4,4(0,R13)
*
         LR    R2,0           AIX: address of argument
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
         LM    R2,R12,16(R13)
         BR    R14
         LTORG
*
*  D A T A   A R E A S
*
STACK    DSECT
SAVEAREA DS    18F
AIXOUT   DS    0F
LINKR23  DS    2F
FPNUM    DS    D
STEND    DS    0D
         END
#ifdef CERNLIB_TCGEN_FLOARG
#undef CERNLIB_TCGEN_FLOARG
#endif
