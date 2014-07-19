*
* $Id: ibits.s,v 1.1.1.1 1996/02/15 17:53:12 mclareni Exp $
*
* $Log: ibits.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:12  mclareni
* Kernlib
*
*
IBITS    CSECT
*
* CERN PROGLIB# M441    BITPAK          .VERSION KERNIBM  2.22  870922
* ORIG.  MAY 87,  K.M.STORR, CERN/DD
*
*     FUNCTION IBITS : J=IBITS(I,N,L)
*                     is  J=ISHFT(ISHFT(I,32-L-N),L-32)
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
IBITS    AMODE ANY
IBITS    RMODE ANY
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
         PRINT NOGEN
*
*  P R O L O G U E
*
         USING IBITS,R15
         B     SAVEREGS
         DC    AL1(7),CL7'IBITS'   IDENTIFIER
         DC    C'1.01'               VERSION.MODIFICATION LEVEL
* VERSION 1.01 CHANGES R2 TO R4 BELOW. 30/07/87, HRR, DD/CERN.
SAVEREGS STM   R14,R4,12(R13)
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
         L     2,0(0,1)      ADDRESS OF I
         L     0,0(0,2)      I
         L     2,4(0,1)      ADDRESS OF N
         L     1,8(0,1)      ADDRESS OF L
         L     1,0(0,1)      L
         ST    1,LENGTH
*
         A     1,0(0,2)      L+N
         LA    2,32
         SR    2,1           32-L-N
         SLL   0,0(2)        SHIFT OUT UNWANTED LHS BITS
*
         LA    2,32
         S     2,LENGTH      32-L
         SRL   0,0(2)        FINAL RESULT
*
*  E P I L O G U E
*
EPILOGUE EQU   *
         L     R13,4(0,R13)
         LM    R1,R4,24(R13)
         MVI   12(R13),X'FF'
         BR    R14
*
*  D A T A   A R E A S
*
LENGTH   DS    F
         LTORG
         END
#ifdef CERNLIB_TCGEN_IBITS
#undef CERNLIB_TCGEN_IBITS
#endif
