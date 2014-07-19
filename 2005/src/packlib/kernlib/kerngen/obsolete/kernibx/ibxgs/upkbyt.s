*
* $Id: upkbyt.s,v 1.1.1.1 1996/02/15 17:54:42 mclareni Exp $
*
* $Log: upkbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:42  mclareni
* Kernlib
*
*
UPKBYT   CSECT
*
* CERN PROGLIB# M422    UPKBYT          .VERSION KERNIBX  1.01  900524
* ORIG.  FEB 83,  R.MATTHEWS, CERN/DD
* Modified for AIX, Roger Howard, February 1990
*
*     CALL UPKBYT (A,JA,IY,N,MPACK)
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
         USING *,R15
         ENTRY _upkbyt_
_upkbyt_ STM   R2,R1,16(R13)
         L     R6,88(,R13)           AIX: address of arg5 = MPACK
         L     R5,0(,R3)             AIX: value of arg4 = N
         LTR   R5,R5          N .LE. 0 ?
         BNP   EPILOGUE       YES, BRANCH
         LR    R4,R2          AIX: address of arg3 = IY
         L     R1,0(,R1)      AIX: value of arg2 = JA
         LR    R2,R0          AIX: address of arg1 = JA
         L     R7,4(0,R6)     R7 := INWORD
         L     R6,0(0,R6)     R6 := NBITS
         LTR   R6,R6          NBITS .GT.0 ?
         BP    L1             YES, BRANCH
         LA    R6,1           R6 := 1   (NBITS)
         LA    R7,32          R7 := 32  (INWORD)
L1       SR    R0,R0
         BCTR  R1,0
         DR    R0,R7
         SLL   R1,2
         LA    R2,0(R1,R2)    R2 -> FIRST PACKED WORD
         LR    R8,R7
         SR    R8,R0          R8 := NUMBER OF 'BYTES' IN FIRST WORD
         LR    R1,R0
         MR    R0,R6
         L     R0,0(0,R2)     R0 := FIRST PACKED WORD
         SRL   R0,0(R1)       SHIFT OUT UNWANTED 'BYTES'
         BCTR  R5,0
         SLL   R5,2
         LA    R5,0(R5,R4)    R5 -> LAST TARGET WORD
         LR    R3,R4          R3 -> FIRST TARGET WORD
         LA    R4,4
         SR    R3,R4
         LA    R9,32
         SR    R9,R6          R9 := NO. OF HIGH ORDER ZEROS IN TARGET
L2       BXH   R3,R4,EPILOGUE R3 -> NEXT TARGET WORD
         SRDL  R0,0(R6)       SHIFT NEXT 'BYTE' INTO TOP OF R1
         SRL   R1,0(R9)       SHIFT IT DOWN TO END OF R1
         ST    R1,0(0,R3)     STORE IT
         BCT   R8,L2          BRANCH TO PROCESS THE NEXT 'BYTE'
         AR    R2,R4          R2 -> NEXT PACKED WORD
         L     R0,0(0,R2)     R0 := NEXT PACKED WORD
         LR    R8,R7          R8 := NUMBER OF 'BYTES' TO EXTRACT
         B     L2
EPILOGUE EQU   *
         LM    R2,R12,16(R13)
         BR    R14
         END
#ifdef CERNLIB_TCGEN_UPKBYT
#undef CERNLIB_TCGEN_UPKBYT
#endif
