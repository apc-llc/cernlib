*
* $Id: qnexte.s,v 1.1.1.1 1996/02/15 17:53:23 mclareni Exp $
*
* $Log: qnexte.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:23  mclareni
* Kernlib
*
*
QNEXTE   CSECT
*
* CERN PROGLIB# Z041    QNEXTE          .VERSION KERNIBM  2.10  840827
*
* FUNCTION:     ON FIRST ENTRY, STANDARD REGISTER SAVING AND SAVE AREA
*               CHAINING ARE PERFORMED AND ROUTINE QNEXT IS CALLED.
*               ON RE-ENTRY, REGISTER SAVING AND
*               SAVE AREA CHAINING ARE NOT PERFORMED BUT QNEXT IS
*               CALLED AFTER APPROPRIATE VALUES ARE PLACED IN THE
*               LINKAGE REGISTERS 13,14,15 AND 1. WHEN CONTROL IS
*               RETURNED BY QNEXT, A STANDARD RETURN TO MAIN IS MADE.
*               AT ALL TIMES IT THEREFORE APPEARS, (E.G. IN A FORTRAN
*               TRACEBACK), AS THOUGH THE CHAIN OF CALLS WAS MAIN,
*               QNEXTE, QNEXT ... , AND A CHAIN OF RETURNS LEADS BACK
*               TO MAIN.
*
* CALLING SEQUENCE:     CALL QNEXTE
*
* ORIGINAL VERSION : R. MATTHEWS, CERN/DD, OCTOBER 1981.
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
QNEXTE   AMODE ANY
QNEXTE   RMODE ANY
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
         SPACE 2
         SPACE
*  P R O L O G U E
         SPACE
         USING QNEXTE,R15
         B     SAVEREGS
         DC    AL1(7),CL7'QNEXTE'    IDENTIFIER
         DC    C'1.00'               VERSION.MODIFICATION LEVEL
SAVEREGS CLI   ENTRY,1        IS THIS THE FIRST ENTRY ?
         BNE   CALL           NO, BRANCH
         MVI   ENTRY,0        TURN OFF THE FIRST ENTRY FLAG
         STM   R14,R12,12(R13)  DO STANDARD SAVE AND S.A. CHAINING...
         LR    R4,R13
         CNOP  0,4
         BAL   R13,AROUNDSA
SAVEAREA DC    A(EPILOGUE),17F'0'
AROUNDSA ST    R13,8(0,R4)
         ST    R4,4(0,R13)
         SPACE 2
*  C A L L   Q N E X T
         SPACE
CALL     EQU   *
         L     R13,=A(SAVEAREA)  ENSURE R13 -> SAVE AREA IN THIS CSECT
         XC    8(4,R13),8(R13)   ZERO THE LOWER S.A. POINTER
         SR    R1,R1             R1 := 0, (NO ARGUMENTS)
         L     R15,=V(QNEXT)     R15 -> QNEXT
         BALR  R14,R15           BRANCH TO QNEXT
         SPACE 2
*  E P I L O G U E
         SPACE
EPILOGUE EQU   *                 PERFORM A STANDARD RETURN...
         L     R13,4(0,R13)
         LM    R14,R12,12(R13)
         MVI   12(R13),X'FF'
         BR    R14
*  D A T A   A R E A S
         SPACE
ENTRY    DC    X'01'             FIRST ENTRY FLAG
         SPACE 2
         LTORG
         SPACE 2
         END
#ifdef CERNLIB_TCGEN_QNEXTE
#undef CERNLIB_TCGEN_QNEXTE
#endif
