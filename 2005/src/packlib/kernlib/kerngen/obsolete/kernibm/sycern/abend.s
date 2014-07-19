*
* $Id: abend.s,v 1.1.1.1 1996/02/15 17:53:19 mclareni Exp $
*
* $Log: abend.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:19  mclareni
* Kernlib
*
*
ABEND    CSECT
*
* CERN PROGLIB# Z035    ABEND           .VERSION KERNIBM  1.11  820328
*
#if defined(CERNLIB_QMIBMXA)
ABEND    AMODE ANY
ABEND    RMODE ANY
#endif
*
* FUNCTION:     ABEND A JOB STEP WITH A USER COMPLETION CODE.
*
* CALLING SEQUENCE:     CALL ABEND(KODEU)
*
* ARGUMENTS:     KODEU  - AN INTEGER CONSTANT OR 4-BYTE INTEGER
*                         VARIABLE WITH A VALUE IN THE RANGE 0-4095.
*                         IF A VALUE OUTSIDE THIS RANGE IS GIVEN,
*                         A VALUE OF 1 WILL BE USED.  THIS ARGUMENT
*                         IS OPTIONAL; IF OMITTED, A USER COMPLETION
*                         CODE OF 1 WILL BE USED.
*
*********************************************************************
*
*                   M O D I F I C A T I O N   L O G
*
* ORIGINAL VERSION : 1.00
*
* R. MATTHEWS, CERN/DD, MAY 1980.
*
*********************************************************************
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
         PRINT NOGEN
*
*  P R O L O G U E
*
         USING ABEND,R15
         B     SAVEREGS
         DC    AL1(7),CL7'ABEND'     IDENTIFIER
         DC    C'1.00'               VERSION.MODIFICATION LEVEL
SAVEREGS STM   R14,R12,12(R13)
         LR    R4,R13
         CNOP  0,4
         BAL   R13,AROUNDSA
SAVEAREA DC    18F'0'
         DROP  R15
         USING SAVEAREA,R13
AROUNDSA ST    R13,8(0,R4)
         ST    R4,4(0,R13)
         XC    8(4,R13),8(R13)
*
         LTR   R1,R1          IS THERE AN ARGUMENT ?
         BZ    DEFAULT        NO, BRANCH
*
         L     R1,0(0,R1)     R1 -> ARGUMENT
         L     R1,0(0,R1)     R1 := USER-SUPPLIED COMPLETION CODE
         CL    R1,=F'4095'    IS IT IN THE RANGE 0-4095 ?
         BNH   ABEND1         YES, BRANCH
*
DEFAULT  LA    R1,1           DEFAULT COMPLETION CODE TO 1
*
ABEND1   ABEND (R1),DUMP      ISSUE ABEND MACRO
*
         LTORG
*
         END
#ifdef CERNLIB_TCGEN_ABEND
#undef CERNLIB_TCGEN_ABEND
#endif
#ifdef CERNLIB_SYOS_REENT
#undef CERNLIB_SYOS_REENT
#endif
