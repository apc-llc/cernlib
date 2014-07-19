*
* $Id: uh1toc.s,v 1.1.1.1 1996/02/15 17:54:42 mclareni Exp $
*
* $Log: uh1toc.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:42  mclareni
* Kernlib
*
*
UH1TOC   CSECT
*
* CERN PROGLIB# M409    UH1TOC          .VERSION KERNIBX  1.01  900524
* ORIG.  NOV 83,  R.MATTHEWS, CERN/DD
* Modified for AIX, Roger Howard, January 1990
*
*     CALL UH1TOC (V1,VC,NCH)
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
         ENTRY _uh1toc_
_uh1toc_ STM   R2,R12,16(R13)
         L     R12,0(,R2)     AIX: value of arg3 = NCH
         LR    R11,R1         AIX: address of arg2 = VC
         LR    R10,R0         AIX: address of arg1 = V1
         LTR   R12,R12        NHC=0 ?
         BZ    EPILOGUE       YES, BRANCH
*
LOOP1    IC    R2,0(0,R10)    GET THE NEXT CHARACTER FROM V1
         STC   R2,0(0,R11)    STORE IT IN VC
         LA    R10,4(0,R10)   R10 -> NEXT INPUT WORD
         LA    R11,1(0,R11)   R11 -> NEXT LOCATION IN VC
         BCT   R12,LOOP1      LOOP TO PROCESS THE NEXT CHARACTER
*
EPILOGUE EQU   *
         LM    R2,R12,16(R13)
         BR    R14
         END
#ifdef CERNLIB_TCGEN_UH1TOC
#undef CERNLIB_TCGEN_UH1TOC
#endif
