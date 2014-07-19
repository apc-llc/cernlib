*
* $Id: intrac.s,v 1.1.1.1 1996/02/15 17:53:21 mclareni Exp $
*
* $Log: intrac.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:21  mclareni
* Kernlib
*
*
INTRAC   CSECT
*
* CERN PROGLIB# Z044    INTRAC          .VERSION KERNIBM  2.11  850320
*
* CALLING SEQUENCE:     VALUE = INTRAC(DUMMY)
*               OR:     IF (INTRAC(DUMMY)) .....
*
* FUNCTION:     A LOGICAL*4 FUNCTION TO DETERMINE WHETHER A
*               PROGRAM IS RUNNING IN A TSO OR BATCH ENVIRONMENT.
*               THE FUNCTION HAS THE VALUE .TRUE. IF RUNNING IN A
*               TSO ENVIRONMENT OTHERWISE THE VALUE IS .FALSE.
*               THE ENVIRONMENT IS DETERMINED BY THE CONTENTS OF
*               ASCBTSB WHICH IS ASSUMED TO CONTAIN ZERO IF IN
*               BATCH AND NON-ZERO IF IN TSO.
*
* ARGUMENTS:     DUMMY  - A DUMMY ARGUMENT TO SATISFY THE COMPILER.
*                         THE ARGUMENT IS NOT CHANGED BY THE
*                         SUBPROGRAM REFERENCE.
*
* R. MATTHEWS, CERN/DD, MAY 1980.
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
INTRAC   AMODE ANY
INTRAC   RMODE ANY
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
*
         USING INTRAC,R15
*
         B     START
*
         DC    AL1(7),CL7'INTRAC'    IDENTIFIER
         DC    C'2.00'               VERSION.MODIFICATION LEVEL
*
START    L     R1,548         R1 -> ASCB
         L     R0,60(0,R1)    R0 := ASCBTSB
         LTR   R0,R0          TEST THE VALUE OF ASCBTSB
         BZR   R14            ZERO, RETURN
*
         LA    R0,1           SET FUNCTION VALUE := .TRUE.
         BR    R14            RETURN
*
         END
#ifdef CERNLIB_TCGEN_INTRAC
#undef CERNLIB_TCGEN_INTRAC
#endif
