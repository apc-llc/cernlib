*
* $Id: intarg.s,v 1.1.1.1 1996/02/15 17:54:37 mclareni Exp $
*
* $Log: intarg.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:37  mclareni
* Kernlib
*
*
INTARG   CSECT
*
* CERN PROGLIB# M250    INTARG          .VERSION KERNIBX  1.01  900523
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
         USING *,R15
         ENTRY _intarg_
_intarg_ STM   R2,R1,16(R13)
         LR    R4,R13
         USING STACK,R13
         S     R13,=A(STEND-STACK)     ... added for AIX
         ST    R4,4(0,R13)
         LR    2,0            R2 -> ARGUMENT
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
         STD   F0,136(0,R13)  STORE THE RESULT
         L     R0,140(0,R13)  R0 := INTEGER PART
         B     EPILOGUE       RETURN
*
NEGATIVE TM    0(R2),X'7F'    IS THE ARGUMENT INTEGER ?
         BO    EPILOGUE       PROBABLY, RETURN
*
         SDR   F0,F0          CLEAR THE LOW-ORDER PART OF F0
         LE    F0,0(0,R2)     F0 := ARGUMENT
         AW    F0,=X'4E00000000000000'  SHIFT OUT THE FRACTIONAL PART
         STD   F0,136(0,R13)  STORE THE RESULT
         L     R0,140(0,R13)  R0 := INTEGER PART
         LNR   R0,R0          NEGATE IT
EPILOGUE L     R13,4(0,R13)
         LM    R2,R12,16(R13)
         BR    R14
         LTORG
*
*  D A T A   A R E A S
*
STACK    DSECT
SAVEAREA DS    18F
AIXOUT   DS    4F
LINKR23  DS    2F
FPNUM    DS    D
STEND    DS    D
         END
#ifdef CERNLIB_TCGEN_INTARG
#undef CERNLIB_TCGEN_INTARG
#endif
