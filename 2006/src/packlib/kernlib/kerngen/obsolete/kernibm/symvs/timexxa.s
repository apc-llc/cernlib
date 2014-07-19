*
* $Id: timexxa.s,v 1.1.1.1 1996/02/15 17:53:19 mclareni Exp $
*
* $Log: timexxa.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:19  mclareni
* Kernlib
*
*
#if defined(CERNLIB_QMIBMXA)
TIMEX    CSECT
*
* CERN PROGLIB# Z007    TIMEX  NEW      .VERSION KERNIBM  2.31  901105
*
*              TIMEX      EXECUTION TIME USED SO FAR (SECONDS)
*
*        There was a problem at DESY: TIMEX specifies AMODE ANY to be
*        able to run "above the line". But it calls the EXTRACT macro.
*        which, with MVS/XA, canNOT run up there.
*        There fore the code was modified by Klaus Tietgen, DESY-R1.
*        Lines added are flagged                                  Tie-A
*        Lines deleted are flagged                                Tie-D
*
*        SPLEVEL  SET=2
TIMEX    AMODE ANY
TIMEX    RMODE ANY
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
*                          FORCE ASCB UPDATING    MVS/SE
         LR    R2,R15                             MVS/SE
         LR    R3,R1                              MVS/SE
         CALLDISP                                 MVS/SE
         LR    R15,R2                             MVS/SE
         LR    R1,R3                              MVS/SE
         USING ASCB,R5              ADDRESSABILITY OF ASCB
         LM    R2,R3,ASCBEJST       ELAPSED JOB STEP TIME
         SRDA  R2,12                BIT 51 = 1 MICROSECOND
         STM   R2,R3,TEMPD          STORE TEMPORARILY
         MVI   TEMPD,X'4E'          MOVE IN EXPONENT
         LD    F0,TEMPD             LOAD FP (UNNORM)
         DD    F0,=D'1000000.'      GET INTO SECONDS
         DROP  R4
         DROP  R5
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
         STM   R4,R2,SVZTI                                        Tie-A
         L     R15,EXPG                                           Tie-A
         LTR   R15,R15                                            Tie-A
         BNZ   EXPGJ                                              Tie-A
         GETMAIN R,LV=EXMUL                                       Tie-A
         ST    R1,EXPG                                            Tie-A
         MVC   0(EXMUL-24,R1),EXMU                                Tie-A
         LR    R15,R1                                             Tie-A
EXPGJ    BASSM R14,R15                                            Tie-A
         LM    R4,R2,SVZTI                                        Tie-A
*        LR    R5,R1               SAVE R1 HERE                   Tie-D
*        EXTRACT CPTR,FIELDS=COMM                                 Tie-D
*        LR    R1,R5               RESTORE CONTENTS OF R1         Tie-D
*        L     R3,CPTR                                            Tie-D
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
SVZTI    DS    15F                                                Tie-A
LAST     DC    D'0'                ELAPSED TIME AT LAST ENTRY
TODCLOCK DS    D                   TOD
TEMPD    DS    D                   TEMPORARY FOR FIX -> FLOAT
*CPTR     DC    F'0'                                              Tie-D
COMECBAD DC    F'0'
EXPG     DC    F'0'                                               Tie-A
EXMU     STM   R14,R2,HV-*(R15)    MUSTERPROGRAMM ZUM KOPIEREN    Tie-A
         BALR  R14,0                                              Tie-A
         DROP  R6                                                 Tie-A
         USING *,R14                                              Tie-A
         LA    R2,HV-4                                            Tie-A
         EXTRACT (2),FIELDS=COMM                                  Tie-A
         L     R3,HV-4             RESULT IN R3                   Tie-A
         LM    R14,R2,HV                                          Tie-A
         BSM   0,R14                                              Tie-A
         DS    0F                                                 Tie-A
HV       EQU   *+4                                                Tie-A
EXMUL    EQU   *-EXMU+24                                          Tie-A
         LTORG
*
         CVT   DSECT=YES,LIST=YES
         IHAASCB DSECT=YES
         END
#ifdef CERNLIB_SYMVS_TIMEXN
#undef CERNLIB_SYMVS_TIMEXN
#endif
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
#endif
