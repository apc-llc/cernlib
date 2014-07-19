*
* $Id: timexn.s,v 1.1.1.1 1996/02/15 17:53:19 mclareni Exp $
*
* $Log: timexn.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:19  mclareni
* Kernlib
*
*
TIMEX    CSECT
*
* CERN PROGLIB# Z007    TIMEX  NEW      .VERSION KERNIBM  1.11  820328
*
*              TIMEX      EXECUTION TIME USED SO FAR (SECONDS)
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
TIMEX    AMODE ANY
TIMEX    RMODE ANY
#endif
         ENTRY TIMED      EXECUTION TIME SINCE LAST CALL (SEC)
         ENTRY TIMEL      EXECUTION TIME LEFT UNTIL TIME LIMIT (S)
*
*      IN CASE OF TROUBLE YOU MAY DELETE THIS DECK,
*      WHICH GIVES YOU THE OLD TIMEX IN THE NEXT DECK
*
*******************************************************************
*
*    A. BERGLUND / CERN      DATE: 27/02/80
*
*       THE STEP TIME LIMIT IS GIVEN BY ASCBJSTL (4 BYTES)
*       IN UNITS OF 1.048576 SECONDS.
*
*       THE ELAPSED TIME UP TO THE LAST DISPATCH IS GIVEN BY
*       ASCBEJST (8 BYTES) WITH BIT 51 = 1 MICROSECOND.
*       IT IS HOWEVER ONLY UPDATED BY THE DISPATCHER WHEN
*       ANOTHER ADDRESS SPACE IS GIVEN CONTROL
*
*       IN MVS/SE UPDATING OF THE ASCBEJST IS FORCED BY
*       AN SVC 137 (CALLDISP) CALL.
*
*       IN A NON SE SYSTEM ONE CAN USE THE FOLLOWING
*       THE BEST ESTIMATE FOR THE CURRENT ELAPSED TIME
*       IS THE ASCBEJST +
*       THE CURRENT TOD - TOD AT LAST DISPATCH (FROM LCCADTOD).
*
*       THE CORRECT CALCULATION IS MADE FOR TIMED ONLY
*       SINCE THE ACCURACY OF THE ASCBEJST IS SOME
*       10'S OF MILLISECONDS - GOOD ENOUGH FOR TIMEX & TIMEL.
*       ACCURACY OF TIMED IS AROUND 50 MICROSECONDS FOR A NON SE
*       SYSTEM. IN MVS/SE THE ACCURACY IS ABOUT 20 MICROSECONDS
*       (RMS OF DISTRIBUTION). TIME BETWEEN TWO CONSECUTIVE CALLS
*       WITH NO CODE IN BETWEEN IS ABOUT 50 MICROSECONDS.
*
*******************************************************************
*
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R13      EQU   13
R14      EQU   14
R15      EQU   15
F0       EQU   0
F2       EQU   2
F4       EQU   4
F6       EQU   6
*
         STM   R2,R5,28(R13)        SAVE REGS
         USING TIMEX,R15            ADDRESSABILITY
         L     R4,CVTPTR
         USING CVT,R4               ADDRESSABILITY OF CVT
         L     R5,CVTTCBP           ADDRESS OF 4 WORD LIST
         L     R5,12(R5)            CURRENT ASCB
         USING ASCB,R5              ADDRESSABILITY OF ASCB
         LM    R2,R3,ASCBEJST       ELAPSED JOB STEP TIME
         SRDA  R2,12                BIT 51 = 1 MICROSECOND
         STM   R2,R3,TEMPD          STORE TEMPORARILY
         MVI   TEMPD,X'4E'          MOVE IN EXPONENT
         LD    F0,TEMPD             LOAD FP (UNNORM)
         DD    F0,=D'1000000.'      GET INTO SECONDS
         L     R2,0(R1)             ADDRESS OF TARGET
         STE   F0,0(R2)             STORE AS REAL*4
         LM    R2,R5,28(R13)        RESTORE REGS
         BR    R14                  & RETURN
         DROP  R15
         DROP  R4
         DROP  R5
*
TIMED    EQU   *             TIME SINCE LAST ENTRY
         USING TIMED,R15
         STM   R2,R5,28(R13)        SAVE REGS
         L     R4,CVTPTR
         USING CVT,R4               ADDRESSABILITY OF CVT
         L     R5,CVTTCBP           ADDRESS OF 4 WORD LIST
         L     R5,12(R5)            CURRENT ASCB
#if defined(CERNLIB_SYMVSSE)
*                          FORCE ASCB UPDATING    MVS/SE
         LR    R2,R15                             MVS/SE
         LR    R3,R1                              MVS/SE
         CALLDISP                                 MVS/SE
         LR    R15,R2                             MVS/SE
         LR    R1,R3                              MVS/SE
#endif
         USING ASCB,R5              ADDRESSABILITY OF ASCB
         LM    R2,R3,ASCBEJST       ELAPSED JOB STEP TIME
         SRDA  R2,12                BIT 51 = 1 MICROSECOND
         STM   R2,R3,TEMPD          STORE TEMPORARILY
         MVI   TEMPD,X'4E'          MOVE IN EXPONENT
         LD    F0,TEMPD             LOAD FP (UNNORM)
         DD    F0,=D'1000000.'      GET INTO SECONDS
         DROP  R4
         DROP  R5
#if !defined(CERNLIB_SYMVSSE)
         L     R4,PSALCCAV-PSA      VIRT ADDRESS OF LCCA
         USING LCCA,R4              ADDRESSABILITY OF LCCA
         LM    R2,R3,LCCADTOD       TOD WHEN TCB IS DESPATCHED
         STCK  TODCLOCK             TOD NOW
         N     R2,=X'7FFFFFFF'      MASK OFF SIGN BIT
         SRDA  R2,12                BIT 51 = 1 MICROSECOND
         STM   R2,R3,TEMPD          STORE TEMPORARILY
         MVI   TEMPD,X'4E'          MOVE IN EXPONENT
         LD    F4,TEMPD
         DD    F4,=D'1000000.'      GET INTO SECONDS
         LM    R2,R3,TODCLOCK       LOAD TODCLOCK
         N     R2,=X'7FFFFFFF'      MASK OFF SIGN BIT
         SRDA  R2,12                BIT 51 = 1 MICROSECOND
         STM   R2,R3,TEMPD          STORE TEMPORARILY
         MVI   TEMPD,X'4E'          MOVE IN EXPONENT
         LD    F6,TEMPD             LOAD FP
         DD    F6,=D'1000000.'      GET INTO SECONDS
         SDR   F6,F4                TOD NOW - TOD AT LAST DISPATCH
         ADR   F0,F6                ADD ON TO ELAPSED AT LAST DISP
#endif
         LDR   F2,F0                SAVE
         SD    F0,LAST              SUBTRACT TIME AT LAST ENTRY
         STD   F2,LAST              SAVE THIS ELAPSED
         L     R5,0(R1)             ADDRESS OF RESULT
         STE   F0,0(R5)             STORE AS REAL*4
         LM    R2,R5,28(R13)        RESTORE REGS
         BR    R14                  & RETURN
         DROP  R15
*
TIMEL    EQU   *         TIME LEFT UNTIL TIME LIMIT
         STM   R2,R6,28(R13)        SAVE REGS
         LR    R6,R15
         USING TIMEL,R6
         L     R4,CVTPTR
         USING CVT,R4               ADDRESSABILITY OF CVT
         L     R5,CVTTCBP           ADDRESS OF 4 WORD LIST
         L     R5,12(R5)            CURRENT ASCB
         USING ASCB,R5              ADDRESSABILITY OF ASCB
         L     R2,ASCBJSTL          JOB STEP TIME LIMIT
         S     R2,ASCBEJST          SUBTRACT OFF TOP HALF OF ELAPSED
         ST    R2,TEMPD+4           STORE TEMPORARILY
         MVC   TEMPD(4),=X'4E000000' MOVE IN EXPONENT HALF
         LD    F2,TEMPD             OAD FP
         MD    F2,=D'1.048576'      INTO SECONDS
         LDR   F0,F2                LOAD & TEST
         BNM   OPSTOP
         SDR   F0,F0                IF NEG SET TO ZERO
         DROP  R4
         DROP  R5
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
STRESULT EQU   *
         L     R4,0(R1)            ADDRESS OF RESULT
         STE   F0,0(R4)            STORE AS REAL*4
         LM    R2,R6,28(R13)       RESTORE REGS
         BR    R14                 & RETURN
LAST     DC    D'0'                ELAPSED TIME AT LAST ENTRY
TODCLOCK DS    D                   TOD
TEMPD    DS    D                   TEMPORARY FOR FIX -> FLOAT
CPTR     DC    F'0'
COMECBAD DC    F'0'
         LTORG
*
         CVT   DSECT=YES,LIST=YES
         IHAASCB DSECT=YES
#if !defined(CERNLIB_SYMVSSE)
         IHALCCA
         IHAPSA
#endif
         END
#ifdef CERNLIB_SYMVS_TIMEX
#undef CERNLIB_SYMVS_TIMEX
#endif
#ifdef CERNLIB_TCGEN_TIMED
#undef CERNLIB_TCGEN_TIMED
#endif
#ifdef CERNLIB_TCGEN_TIMEL
#undef CERNLIB_TCGEN_TIMEL
#endif
#ifdef CERNLIB_TCGEN_TIMEX
#undef CERNLIB_TCGEN_TIMEX
#endif
