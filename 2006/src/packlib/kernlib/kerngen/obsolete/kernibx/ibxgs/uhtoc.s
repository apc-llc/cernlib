*
* $Id: uhtoc.s,v 1.1.1.1 1996/02/15 17:54:42 mclareni Exp $
*
* $Log: uhtoc.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:42  mclareni
* Kernlib
*
*
UHTOC    CSECT
*
* CERN PROGLIB# M409    UHTOC           .VERSION KERNIBX  1.01  900524
* ORIG.  NOV 83,  R.MATTHEWS, CERN/DD
* Modified for AIX, Roger Howard, January 1990
*
*     CALL UHTOC (VI,NH,VC,NCH)
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
*  P R O L O G U E
*
         USING *,R15
         ENTRY _uhtoc_
_uhtoc_  STM   R2,R12,16(R13)
         L     R12,0(,R3)     AIX: value of arg4 = NCH
         LR    R11,R2         AIX: address of arg3 = VC
         L     R10,0(,R1)     AIX: value of arg2 = NH
         LR    R9,R0          AIX: address of arg1 = VI
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
EPILOGUE EQU   *
         LM    R2,R12,16(R13)
         BR    R14
         END
#ifdef CERNLIB_TCGEN_UHTOC
#undef CERNLIB_TCGEN_UHTOC
#endif
