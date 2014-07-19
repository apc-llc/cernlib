*
* $Id: timex.s,v 1.1.1.1 1996/02/15 17:53:20 mclareni Exp $
*
* $Log: timex.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:20  mclareni
* Kernlib
*
*
TIMEX    CSECT
*
* CERN PROGLIB# Z007    TIMEX           .VERSION KERNIBM  1.04  781218
*
*    ENTRY  TIMEX                      .ELAPSED JOB STEP TIME
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
TIMEX    AMODE ANY
TIMEX    RMODE ANY
#endif
     ENTRY  TIMED                      .CP TIME SINCE LAST CALL
     ENTRY  TIMEL                      .REMAINING JOB STEP TIME
*
* THIS IS THE MOST SIMPLE MVS VERSION OF TIMED REQUIRING ONLY
* ADDRESSIBILITY TO THE ASCB CONTROL BLOCK. THE ACCURACY FOR ALL
* ENTRY POINTS IS HENCE 10'S OF MILLISECONDS WITH UPDATES ONLY WHEN
* DISPATCHED HENCE VERY INNACURATE (BY SECONDS) RESULTS ARE POSSIBLE.
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
F0       EQU   0
F2       EQU   2
*
         USING TIMEX,R12           SET ADDRESSABILITY
         BC    15,12(R15)
         DC    X'7'
         DC    CL7'TIMEX  '
         STM   R2,R12,28(R13)
         LR    R12,R15
         B     BEGIN
         DS    0F                  FORCE ALIGNMENT ON WORD BOUNDARY
TIMED    BC    15,16(R15)
         DC    X'7'
         DC    CL7'TIMED  '
         DC    A(TIMEX)
         STM   R2,R12,28(R13)
         L     R12,12(R15)
         B     BEGIN
TIMEL    BC    15,16(R15)
         DC    X'7'
         DC    CL7'TIMEL  '
         DC    A(TIMEX)
         STM   R2,R12,28(R13)
         L     R12,12(R15)
         USING CVT,R2              ESTABLISH CVT ADDRESSABILITY
         USING ASCB,R4             ESTABLISH ASCB ADDRESSABILITY
*
BEGIN    L     R2,CVTPTR           GET CVT POINTER
         L     R3,CVTTCBP          GET ADDRESS OF 4-WORD LIST
         L     R4,12(R3)           GET CURRENT ASCB ADDRESS
         LM    R6,R7,ASCBEJST      GET ELAPSED JOB STEP TIME
         SRDA  R6,12               (R6,R7) BIT 51 = 1 MICROSECOND
         D     R6,=F'100'          CONVERT INTO TENTHS OF MILLISECOND
         X     R7,FOURE+4
         ST    R7,TEMPF+4
         LD    F0,TEMPF
         SD    F0,FOURE
         DE    F0,=E'10000.'       CONVERT INTO SECONDS
         CLI   9(R15),C'X'
         BE    STRESULT
*
         CLI   9(R15),C'L'
         BE    TIMLEFT
         LER   F2,F0
         SE    F0,LAST             SUBTRACT PRECEDING TIME VALUE
         STE   F2,LAST             STORE NEW TIME VALUE
         B     STRESULT
*
TIMLEFT  L     R11,ASCBJSTL        GET JOB STEP TIME LIMIT
         X     R11,FOURE+4
         ST    R11,TEMPF+4
         LD    F2,TEMPF
*                               TIME IS ACTUALLY IN 1.048576 SECS
*                               CF PRINC. OF OPER. PG 47...
*                               MOD. BY ANDERS BERGLUND OCTOBER 1978
         SD    F2,FOURE            JOB STEP TIME LIMIT (FLOATING POINT
         MD    F2,=D'1.048576'
         SER   F2,F0               REMAINING JOB STEP TIME
         LER   F0,F2
         BNM   OPSTOP
         SER   F0,F0
*---SET REMAINING TIME TO ZERO IF OPERATOR HAS ISSUED A STOP COMMAND
*---MODIFIED BY WIEGANDT/RICHARDS JAN 17 1977
OPSTOP   ICM   R3,15,COMECBAD
         BNZ   TESTECB
         LR    R5,R1               SAVE R1 HERE
         EXTRACT CPTR,FIELDS=COMM
         LR    R1,R5               RESTORE CONTENTS OF R1
         L     R3,CPTR
         L     R3,0(R3)            ADDRESS(COMMUNICATIONS ECB)
         ST    R3,COMECBAD
TESTECB  TM    0(R3),X'40'         ECB POSTED
         BNO   STRESULT
         SER   F0,F0
STRESULT L     R9,0(R1)
         STE   F0,0(R9)            STORE RESULT
         LM    R2,R12,28(R13)
         SR    R15,R15             SET RETURN CODE
         BCR   15,R14              RETURN
*
         DS    0D            FORCE ALIGNMENT ON DOUBLEWORD BOUNDARY
FOURE    DC    X'4E000000'
         DC    X'80000000'
TEMPF    DC    X'4E000000'
         DC    F'0'
LAST     DC    F'0'
CPTR     DC    F'0'
COMECBAD DC    F'0'                INITIALIZE TO ZERO
PATCH    DC    64S(*)              PATCH AREA
*
*      CVT   DSECT=YES,LIST=YES
       CVT   DSECT=YES,LIST=NO
      IHAASCB DSECT=YES
       END
#ifdef CERNLIB_TCGEN_TIMED
#undef CERNLIB_TCGEN_TIMED
#endif
#ifdef CERNLIB_TCGEN_TIMEL
#undef CERNLIB_TCGEN_TIMEL
#endif
#ifdef CERNLIB_TCGEN_TIMEX
#undef CERNLIB_TCGEN_TIMEX
#endif
