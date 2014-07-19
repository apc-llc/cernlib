*
* $Id: kaxageml.s,v 1.1.1.1 1996/03/08 11:40:55 mclareni Exp $
*
* $Log: kaxageml.s,v $
* Revision 1.1.1.1  1996/03/08 11:40:55  mclareni
* Kapack
*
*
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_QMIBMXA))
         TITLE 'KAXAGE'
***********************************************************************
*                                                                     *
*                             K A X A G E                             *
*                             ===========                             *
*                                                                     *
***********************************************************************
*                                                                     *
* FUNCTION:     PUT THE TASK INTO A WAIT STATE FOR A SPECIFIED        *
*               PERIOD OF TIME                                        *
*                                                                     *
* CALLING SEQUENCE:   CALL KAXAGE(INTVAL)                             *
*                                                                     *
* ARGUMENTS:     INTVAL - A 4-BYTE INTEGER CONTAINING THE TIME PERIOD,*
*                         IN HUNDREDTHS OF SECONDS, FOR WHICH THE     *
*                         TASK IS TO WAIT. A ZERO OR NEGATIVE VALUE   *
*                         WILL CAUSE AN IMMEDIATE RETURN TO THE       *
*                         CALLING ROUTINE WITH NO WAIT.               *
*                                                                     *
* EXAMPLE:     TO PUT THE TASK INTO A WAIT STATE FOR 5 SECONDS:       *
*                                                                     *
*                        CALL KAXAGE(500)                             *
*                                                                     *
***********************************************************************
         EJECT
***********************************************************************
*                                                                     *
*                   M O D I F I C A T I O N   L O G                   *
*                   ===============================                   *
*                                                                     *
***********************************************************************
*                                                                     *
*                               1 . 0 0                               *
*                               =======                               *
*                                                                     *
* ORIGINAL VERSION.                                                   *
*                                                                     *
* R. MATTHEWS, CERN/DD, MAY 1985.                                     *
*                                                                     *
***********************************************************************
         EJECT
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
         EJECT
         PRINT NOGEN
KAXAGE   CSECT
         SPACE 2
*  P R O L O G U E
         SPACE
         USING KAXAGE,R15
         B     SAVEREGS
         DC    AL1(7),CL7'KAXAGE'    IDENTIFIER
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
         SPACE
         LR    R11,R1         R11 -> ARGUMENT LIST
         SPACE
         L     R4,0(0,R11)    R4 -> USER'S WAIT INTERVAL
         L     R3,0(0,R4)     R3 := USER'S WAIT INTERVAL
         LTR   R3,R3          IS IT .LE. ZERO ?
         BNP   EPILOGUE       YES, BRANCH
         SPACE
         STIMER WAIT,BINTVL=(R4)  ISSUE THE WAIT
         SPACE 2
*  E P I L O G U E
         SPACE
EPILOGUE EQU   *
         L     R13,4(0,R13)
         LM    R14,R12,12(R13)
         MVI   12(R13),X'FF'
         BR    R14
         SPACE 2
         END
#endif
