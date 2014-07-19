*
* $Id: uhtoc.s,v 1.1.1.1 1996/02/15 17:53:17 mclareni Exp $
*
* $Log: uhtoc.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:17  mclareni
* Kernlib
*
*
UHTOC    CSECT
*
* CERN PROGLIB# M409    UHTOC           .VERSION KERNIBM  2.07  831205
* ORIG.  NOV 83,  R.MATTHEWS, CERN/DD
*
*     CALL UHTOC (VI,NH,VC,NCH)
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
UHTOC    AMODE ANY
UHTOC    RMODE ANY
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
         PRINT NOGEN
*
*  P R O L O G U E
*
         USING UHTOC,R15
         B     SAVEREGS
         DC    AL1(7),CL7'UHTOC'     IDENTIFIER
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
         LM    R9,R12,0(R1)   R9  -> VI;  R10 -> NH
*                             R11 -> VC;  R12 -> NCH
         L     R10,0(0,R10)   R10 := NH
         L     R12,0(0,R12)   R12 := NCH
         LTR   R12,R12        NHC=0 ?
         BZ    EPILOGUE       YES, BRANCH
         LA    R4,4           R4 := 4
         CR    R10,R4         IS NH > 4
         BNH   LOOP1          NO, BRANCH
         LR    R10,R4         DEFAULT NH=4
*
LOOP1    L     R3,0(0,R9)     R3 := NEXT INPUT WORD
         LR    R5,R10         R5 := NH
         CR    R5,R12         IS NH > NUMBER OF CHARACTERS REMAINING ?
         BNH   LOOP2          NO, BRANCH
         LR    R5,R12         R5 := NUMBER OF CHARACTERS REMAINING
*
LOOP2    SLDL  R2,8           SHIFT ONE CHARACTER INTO R2
         STC   R2,0(0,R11)    STORE IT IN VC
         LA    R11,1(0,R11)   R11 -> NEXT LOCATION IN VC
         BCT   R5,LOOP2       LOOP TO PROCESS THE NEXT CHARACTER
*
         AR    R9,R4          R9 -> NEXT INPUT WORD
         SR    R12,R10        R12 := NUMBER OF CHARACTERS REMAINING
         BP    LOOP1          LOOP TO PROCESS THE NEXT WORD
*
*  E P I L O G U E
*
EPILOGUE EQU   *
         L     R13,4(0,R13)
         LM    R14,R12,12(R13)
         MVI   12(R13),X'FF'
         BR    R14
         END
#ifdef CERNLIB_TCGEN_UHTOC
#undef CERNLIB_TCGEN_UHTOC
#endif
