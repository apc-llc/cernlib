*
* $Id: intrac.s,v 1.1.1.1 1996/02/15 17:51:48 mclareni Exp $
*
* $Log: intrac.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:48  mclareni
* Kernlib
*
*
         PRINT NOGEN
INTRAC   CSECT
#if defined(CERNLIB_QMIBMXA)
INTRAC   AMODE ANY
INTRAC   RMODE ANY
#endif
*
* CERN PROGLIB# Z044    INTRAC          .VERSION KERNIBM  2.25  881103
*
* LOGICAL FUNCTION WHICH RETURNS THE VALUE 'TRUE' IF THE VIRTUAL
* MACHINE IS RUNNING INTERACTIVELY, (I.E. IT HAS A VIRTUAL CONSOLE
* WHICH IS ASSOCIATED WITH A REAL DEVICE); AND THE VALUE 'FALSE' IF
* IT IS RUNNING NON-INTERACTIVELY, (I.E. A VIRTUAL CONSOLE DOES NOT
* EXIST OR IS NOT ASSOCIATED WITH A REAL DEVICE).
*
*********************************************************************
*
*                   M O D I F I C A T I O N   L O G
*
* 1.00  ORIGINAL VERSION.
* R. MATTHEWS, UNIVERSITE DE GENEVE, OCTOBRE 1988.
*
*********************************************************************
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
*  P R O L O G U E
*
         USING INTRAC,R15
         B     SAVEREGS
         DC    AL1(7),CL7'INTRAC'    IDENTIFIER
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
         SR    R2,R2                    R2 := 0
         BCTR  R2,0                     R2 := -1
         DIAG  R2,R3,X'24'              ISSUE DIAGNOSE CODE X'24'
         BC    3,DISCO                  BRANCH IF CC=2 OR CC=3
         LA    R0,1                     FUNCTION VALUE = 1
         B     EPILOGUE                 RETURN
DISCO    SR    R0,R0                    FUNCTION VALUE = 0
*
*  E P I L O G U E
*
EPILOGUE EQU   *
         L     R13,4(0,R13)
         LM    R14,R15,12(R13)
         LM    R1,R12,24(R13)
         MVI   12(R13),X'FF'
         BR    R14
         END
#ifdef CERNLIB_TCGEN_INTRAC
#undef CERNLIB_TCGEN_INTRAC
#endif
