*
* $Id: timexce.s,v 1.1.1.1 1996/02/15 17:51:45 mclareni Exp $
*
* $Log: timexce.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:45  mclareni
* Kernlib
*
*
#if defined(CERNLIB_CERN)
         PRINT OFF
         MACRO
&NAME    GETCPUTM
&NAME    DIAG  R12,R12,X'000C'    Return time information
         MVI   TOTCPU,X'4E'       Unnormalised FP number
         LD    F0,TOTCPU
         DD    F0,=D'1.0E6'       Microseconds -> Seconds
         TM    FLAGS,INBATCH      In batch?
         BO    *+4
         SD    F0,CPUSTART        No, Adjust time
         MEND
         PRINT ON,NOGEN
TIMEX    CSECT
#if defined(CERNLIB_QMIBMXA)
TIMEX    AMODE ANY
TIMEX    RMODE ANY
#endif
*
* CERN PROGLIB# Z007    TIMEX
*
*********************************************************************
*
*  TITLE : TIMEL/TIMEX/TIMED/TIMEST CERN routines for VM/CMS SLAC batch
*  =====
*
*  FUNCTION : Returns the time used by or left to a job in seconds.
*  ========   The job may be running in SLAC batch or online. An
*             interface to the SLAC JOBSTOP command to return no
*             time left is included.
*
*  DESCRIPTION
*  ===========
*
*  See CERN library specification for Z007 for TIMED/TIMEL/TIMEX.
*
*  TIMEST usage:    CALL TIMEST(TIME)
*  where TIME is a REAL*4 number specifying the time limit in seconds
*  for the job, relative to the current CPU time.
*  Subsequent calls to TIMEST are ignored.
*  In SLAC batch, all calls to TIMEST are ignored.
*
*********************************************************************
*
*                   M O D I F I C A T I O N   L O G
*
* Original Version:
*
* H.R.Renshall CERN/DD February 1985
* Based on the Rutherford Lab version for IBM Batch by C.P.Wood RL
*
* Modified 17 05 85 to return TIMEL=9999. by default
* KERNIBM 2.12 CNL 179                    H.R.Renshall/CERN/DD
*
* Modified 21 06 85 to use the normalised time limit plus the
* CPU normalisation constant now returned by JOB$VM
* KERNIBM 2.13 CNL 180                    H.R.Renshall/CERN/DD
*
* Modified extensively 15/03/89 to use the now public JOB$VM routine.
* Code considerably simplified (My opinion at least...)
* KERNCMS 1.02 CNL 195                    Tony Cass CERN/DD
*
* Modified 15/03/89 to use the new HEPVM JOBSTOP check
* KERNCMS 1.05 CNL 200                    Tony Cass CERN/DD
*
*********************************************************************
*
*
*  P R O L O G U E
*
         USING NUCON,R0
*
         USING TIMEX,R15
         B     16(R15)
         DC    AL1(7),CL7'TIMEX'  Identifier
         DC    C'2.00'              Version.Modification level
         OI    FLAGS,EPTIMEX      Identify Entry Point
         LA    R15,START
         USING START,R15
         BR    R15
*
         ENTRY TIMEL
TIMEL    EQU   *
         USING TIMEL,R15
         B     12(R15)
         DC    X'7'
         DC    CL7'TIMEL'
         OI    FLAGS,EPTIMEL      Identify Entry Point
         LA    R15,START
         USING START,R15
         BR    R15
*
         ENTRY TIMED
TIMED    EQU   *
         USING TIMED,R15
         B     12(R15)
         DC    X'7'
         DC    CL7'TIMED'
         OI    FLAGS,EPTIMED      Identify Entry Point
         LA    R15,START
         USING START,R15
         BR    R15
*
         ENTRY TIMEST
TIMEST   EQU   *
         USING TIMEST,R15
         B     12(R15)
         DC    X'7'
         DC    CL7'TIMEST'
         OI    FLAGS,EPTIMEST     Identify Entry Point
         LA    R15,START
         USING START,R15
*
START    STM   R14,R12,12(R13)
         LR    R4,R13
         CNOP  0,4
         BAL   R13,AROUNDSA
SAVEAREA DC    A(RETURN),17F'0'
         DROP  R15
         USING SAVEAREA,R13
AROUNDSA ST    R13,8(0,R4)
         ST    R4,4(0,R13)
         XC    8(4,R13),8(R13)
*
REALCODE L     R11,0(,R1)         Argument address
         LA    R12,TIMEAREA       For Diagnose X'000C'
         USING TIMEAREA,R12
*
*********************************************************************
* Access CMS constant areas. Call CERN library routine JOB$VM
* to see if interactive or batch (and find time limit if batch)
*********************************************************************
*
         TM    FLAGS,JOBDONE      How's the initialisation?
         BO    WHICHEP
         LA    R1,ARG$VM
         L     R15,=V(JOB$VM)
         BALR  R14,R15
         OI    FLAGS,JOBDONE
         LTR   R0,R0              Are we in batch?
         BZ    WHICHEP            No...
         LD    F0,BATMXTME        Limit in accounting seconds
         DD    F0,BATTFACT        Convert to native seconds
         STD   F0,BATMXTME
         OI    FLAGS,INBATCH      Yes, set the flags
         OI    FLAGS,SETTIME      Make's life simpler
*
WHICHEP  TM    FLAGS,EPTIMEST     Call to TIMEST?
         BNO   REALWORK           No...
*
*********************************************************************
*                 T I M E S T  CODE SECTION
*********************************************************************
*
         TM    FLAGS,SETTIME      Previous call to TIMEST?
         BO    RETURN             Yes - ignore this call
*
         GETCPUTM                 Get time used since session began
         STD   F0,CPUSTART        Store it
         MVC   BATMXTME(4),0(R11) Store user limit
         OI    FLAGS,SETTIME      Show it's all been done
         B     RETURN             And that's all
*
*********************************************************************
*        T I M E X  /  T I M E L    CODE SECTION
*********************************************************************
*
REALWORK TM    FLAGS,EPTIMED      TIMED call?
         BO    DOTIMED            Yes...
*
         LD    F0,=D'0.0'         Default time used so far
         TM    FLAGS,EPTIMEL      For TIMEL, check if JOBSTOP is set
         BNO   NOTFORCD           Not TIMEL
         TM    BATFLAG2,BATSTOP   New, official HEPVM test
#if defined(CERNLIB_QMNOGVAR)
         BNO   NOTFORCD           Not BATCH JOPSTOPped...
#endif
#if !defined(CERNLIB_QMNOGVAR)
         BO    FORCED
*                                 Keep old test for now
         L     R15,=V(JOBSTP)     Have we been forcibly timed out?
         BALR  R14,R15
         LTR   R0,R0
         BZ    NOTFORCD           No....
#endif
FORCED   LD    F0,BATMXTME        Yes, pretend we're at time limit
         B     GOTTIME
*
NOTFORCD TM    FLAGS,SETTIME      Do we want real limits?
         BNO   GOTTIME            No, 0 CPU used is fine and dandy
*
         GETCPUTM                 Interrogate timer
*
GOTTIME  TM    FLAGS,EPTIMEL      Want time used or time left?
         BNO   RETTIME            Time used - but got that already
*
         SD    F0,BATMXTME        Difference between limit and
         LCDR  F0,F0              Current CPU time gives time left
         B     RETTIME
*
*********************************************************************
* Code for  T I M E D
*********************************************************************
*
DOTIMED  GETCPUTM                 Get current CPU time
         LDR   F2,F0
         SD    F0,LASTIMED        Subtract time at last call
         STD   F2,LASTIMED        And reset
*
*********************************************************************
* Return time to user
*********************************************************************
*
RETTIME  STE   F0,0(R11)
*
*********************************************************************
* Common return point
*********************************************************************
*
RETURN   EQU   *
         NI    FLAGS,X'0F'        Clear Entry Point identifier
         L     R13,4(0,R13)
         LM    R14,R12,12(R13)
         MVI   12(R13),X'FF'
         BR    R14
*
*
*********************************************************************
* DATA AREA.
*********************************************************************
*
         DS    0D
BATUSER  DS    CL8                 Sender's UserID
BATACNT  DS    CL8                 Sender's Account
BATBIN   DS    CL8                 Sender's Dist code
BATMACH  DS    CL8                 Virtual Machine ID
BATJOBID DS    CL8                 Job ID
VSTORE   DS    CL8
BATSTRST DS    CL8                 Start/Restart
BATNODE  DS    CL8                 Master NodeID
BATMONID DS    CL8                 BMON ID
BATMASTR DS    CL8                 Master monitor ID
BATMXTME DS    D                   Max CPU time (in accounting seconds)
BATTFACT DS    D                   DP CPU time normalisation factor
MACHTYPE DS    D
         DS    3D
*
ARG$VM   DC    A(BATUSER)
*
FLAGS    DC    XL1'00'
JOBDONE  EQU   X'01'              Show JOB$VM has been called
INBATCH  EQU   X'02'              Distinguish batch/interactive
SETTIME  EQU   X'04'              Show if TIMEST has been called
NOTUSED  EQU   X'08'
EPTIMEX  EQU   X'10'              Entry point TIMEX
EPTIMEL  EQU   X'20'                          TIMEL
EPTIMED  EQU   X'40'                          TIMED
EPTIMEST EQU   X'80'                          TIMEST
*
         LTORG                                                   170583
*
         DS    0D
TIMEAREA EQU   *                  Data area for Pseudo Timer
         DS    2CL8' '            Date and time in character format
VIRTCPU  DC    D'0.0'             Virtual CPU time used
TOTCPU   DC    D'0.0'             Total CPU time used
CPUSTART DC    D'0.0'             Online CPU start time
LASTIMED DC    D'0.0'             CPU used at last TIMED call
*
         REGEQU
         NUCON
         END
#endif
