*
* $Id: timex.F,v 1.1.1.1 1996/02/15 17:51:46 mclareni Exp $
*
* $Log: timex.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:46  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
#if !defined(CERNLIB_CERN)
TIMEX    CSECT
*
* CERN PROGLIB# Z007    TIMEX           .VERSION KERNIBM  2.11  850320
*
*  TITLE : TIMEL/TIMEX/TIMED/TIMEST CERN ROUTINES FOR CMS
*  =====
*
*  FUNCTION : RETURNS THE TIME USED BY OR LEFT TO A JOB IN SECONDS.
*  ========   THE JOB MAY BE RUNNING IN CMS BATCH OR ONLINE.
*
*  AUTHOR : C. P. WOOD, USER SUPPORT, CD RAL, 26TH APRIL 1982
*  ======
*
*  DESCRIPTION
*  ===========
*  SEE CERN LIBRARY SPECIFICATION FOR Z007 FOR TIMED/TIMEL/TIMEX.
*  TIMEST USAGE:
*     CALL TIMEST(TIME,IFLAG)
*  WHERE TIME IS A REAL*8 NUMBER SPECIFYING THE TIME LIMIT IN SECONDS
*  FOR THE JOB, RELATIVE TO THE CURRENT CPU TIME. IF IFLAG IS EQUAL
*  TO ONE, THEN IF A CALL IS MADE TO TIMEX AFTER THE TIME LIMIT HAS
*  EXPIRED, AN ABEND WILL BE FORCED. IF IFLAG IS EQUAL TO ZERO, THEN
*  EXECUTION WILL CONTINUE. THE IFLAG ARGUMENT IS OPTIONAL AND IS
*  DEFAULTS TO ONE. SUBSEQUENT CALLS TO TIMEST ARE IGNORED. IN CMS
*  BATCH, ALL CALLS TO TIMEST ARE IGNORED.
*
*********************************************************************
*  MODIFICATION LOG:
*    17/5/83 CHECK WHETHER RUNNING IN BATCH MODE BY TESTING FOR
*            PRESENCE OF JOB LIMIT CONTROL BLOCK INSTEAD OF TESTING
*            BATCH FLAG IN NUCLEUS. THIS IS FOR THE NEW (BATMAN
*            AND ROBIN) BATCH SYSTEM, WHICH DOES NOT SET THE FLAG.
*********************************************************************
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=1
*TIMEX    AMODE 31
*TIMEX    RMODE ANY
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
FR0      EQU   0
FR2      EQU   2
FR4      EQU   4
FR6      EQU   6
*
         USING TIMEX,R15
         B     12(R15)
         DC    X'7'
         DC    CL7'TIMEX'
EPTIMEX  EQU   X'00'
         MVI   EPNAME,EPTIMEX     IDENTIFY ENTRY POINT
         LA    R15,START
         BR    R15
*
         ENTRY TIMEL
TIMEL    EQU   *
         USING TIMEL,R15
         B     12(R15)
         DC    X'7'
         DC    CL7'TIMEL'
EPTIMEL  EQU   X'01'
         MVI   EPNAME,EPTIMEL     IDENTIFY ENTRY POINT
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
EPTIMED  EQU   X'02'
         MVI   EPNAME,EPTIMED     IDENTIFY ENTRY POINT
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
EPTIMEST EQU   X'03'
         MVI   EPNAME,EPTIMEST    IDENTIFY ENTRY POINT
         LA    R15,START
         USING START,R15
*
START    SAVE  (14,12)            COMMON CODE SECTION
         LR    R10,R1             SAVE ARGUMENT LIST ADDRESS
*
*********************************************************************
* ACCESS CMS CONSTANT AREAS. USED TO CHECK WHETHER PROGRAM IS RUNNING
* IN CMS BATCH AND YIELDS TIME INFORMATION IF IT IS.
*********************************************************************
*
         USING NUCON,0            CMS NUCLEUS CONSTANT AREA
         USING BATLSECT,R2        CMS BATCH CONSTANT AREA
         L     R2,ABATLIMT
*
*********************************************************************
* BRANCH TO DIFFERENT CODE FOR TIMED/TIMEL/TIMEX.
*********************************************************************
*
         CLI   EPNAME,EPTIMEST
         BNE   TEST
*
*********************************************************************
*                 T I M E S T  CODE SECTION
*********************************************************************
*
         C     R2,=F'0'           TIMEST IS IGNORED IN CMS BATCH 170583
         BNE   RETURN                                            170583
*
         CLI   TIMESTCL,CALLED    PREVIOUS CALL TO TIMEST?
         BE    RETURN             YES - IGNORE CALL
*
         MVI   TIMESTCL,CALLED    INDICATE TIMEST CALLED
         BAL   R12,GETCPUTM       GET TIME USED SINCE SESSION BEGAN
         STD   FR0,CPUSTART       STORE IT
         L     R11,0(R10)         FETCH FIRST ARGUMENT ADDRESS
         AD    FR0,0(R11)         ADD USER LIMIT
         STD   FR0,CPUEND         STORE IMAGINARY TIME LIMIT
         MVI   ABENDFLG,ABENDDEF  DEFAULT VALUE FOR ABENDFLG
         LTR   R11,R11            TEST FOR SECOND ARGUMENT
         BM    RETURN
         L     R11,4(R10)         FETCH SECOND ARGUMENT ADDRESS
         MVC   ABENDFLG(1),3(R11) SET ABEND FLAG
         B     RETURN
*
*********************************************************************
*        T I M E X / T I M E L / T I M E D   CODE SECTION
*********************************************************************
*
TEST     L     R11,0(R10)         FETCH ARGUMENT ADDRESS
         C     R2,=F'0'           SEPARATE CODE FOR BATCH        170583
         BNE   CPULFTBT                                          170583
*
         CLI   TIMESTCL,CALLED    MUST BE PREVIOUS CALL TO TIMEST
         BE    CPULFTOL           WHEN RUNNING ONLINE
         LD    FR0,DEFAULT        RETURN DEFAULT IF THERE IS NOT
         B     RETTIME
*
*********************************************************************
* CODE FOR TIMEX/TIMEL/TIMED WHEN RUNNING ONLINE.
*********************************************************************
*
CPULFTOL BAL   R12,GETCPUTM       GET CURRENT CPU TIME
         CD    FR0,CPUEND         HAS TIME LIMIT EXPIRED?
         BM    NOABEND            NO
         CLI   ABENDFLG,X'00'     YES - DOES USER WANT ABEND?
         BE    NOABEND            NO
*
         WTO   '***TIMED/TIMEL/TIMEX*** TIME LIMIT EXCEEDED',ROUTCDE=11
         DMSABN 322               ISSUE MESSAGE AND FORCE ABEND
*
NOABEND  CLI   EPNAME,EPTIMEL     WANT TIME USED OR TIME LEFT?
         BNE   CPUUSDOL           BRANCH FOR TIMEL
*
         SD    FR0,CPUEND         DIFFERENCE BETWEEN LIMIT AND
         LCDR  FR0,FR0            CURRENT CPU TIME GIVES TIME LEFT
         B     RETTIME
*
CPUUSDOL SD    FR0,CPUSTART       SUBTRACT START TIME FROM CURRENT
         B     RETTIME            TO GIVE TIME USED
*
*********************************************************************
* CODE FOR TIMED/TIMEL/TIMEX WHEN RUNNING IN CMS BATCH.
*********************************************************************
*
CPULFTBT BAL   R12,GETCPUTM       GET CURRENT CPU TIME
         CLI   EPNAME,EPTIMEL     TIME USED OR TIME LEFT?
         BNE   CPUUSDBT           BRANCH FOR TIMEL
*
         LH    R0,BATCPUL         BATCH LIMIT
         ST    R0,TIME+4          CONVERT INTEGER TO FLOATING POINT
         LD    FR2,TIME
         AD    FR2,ZERO           NORMALISE IT
         SDR   FR2,FR0            MINUS BATCH TIME USED GIVES TIME
         LDR   FR0,FR2            REMAINING IN SECONDS
*
CPUUSDBT EQU   *                  RETURN TIME USED AS IT IS
*
*********************************************************************
* RETURN TIME TO USER - MORE TO DO IF CALL WAS TO TIMED
*********************************************************************
*
RETTIME  CLI   EPNAME,EPTIMED     SPECIAL CODE FOR TIMED
         BNE   TIMEEND
         LDR   FR2,FR0            SUBTRACT TIME USED AT LAST TIMED
         SD    FR0,LASTTIME       CALL FROM CURRENT TIME USED
         STD   FR2,LASTTIME       SAVE CURRENT TIME FOR NEXT CALL
TIMEEND  STE   FR0,0(R11)         RETURN TIME TO USER
*
*********************************************************************
* COMMON RETURN POINT
*********************************************************************
*
RETURN   RETURN (14,12),T,RC=0
*
*********************************************************************
* SUBROUTINE TO FIND TOTAL CPU TIME USED SINCE START OF SESSION.
* TIME IN SECONDS IS RETURNED IN FR0. ONLY R1 IS CHANGED BESIDES FR0.
* INTERROGATES PSEUDO TIMER TO GET TIME USING DIAGNOSE CODE '0C'.
*********************************************************************
*
GETCPUTM LA    R1,TIMEAREA        POINT TO DATA AREA
         DIAG  R1,R1,X'000C'      INTERROGATE PSEUDO TIMER
*
         MVC   TIME+1(7),TIMEAREA+TOTCPU+1  CONVERT DOUBLE WORD
         LD    FR0,TIME           UNSIGNED INTEGER GIVING TIME IN
         DD    FR0,MUSTOSEC       MICROSECONDS TO REAL*8 GIVING
         BR    R12                TIME IN SECONDS
*
*********************************************************************
* DATA AREA.
*********************************************************************
TIMEAREA DS    4D                 DATA AREA FOR PSEUDO TIMER
TOTCPU   EQU   24                 OFFSET IN AREA FOR TOTAL CPU TIME
TIME     DC    XL4'4E000000'      DATA FOR CONVERTING INTEGER TO REAL
         DS    F
MUSTOSEC DC    D'1.0E6'           MICROSECONDS TO SECONDS FACTOR
CPUSTART DS    D                  ONLINE CPU START TIME
CPUEND   DS    D                  ONLINE CPU END TIME
LASTTIME DC    D'0.0'             CPU USED AT LAST TIMED CALL
DEFAULT  DC    D'0.0'             ONLINE CPU DEFAULT IF NO TIMEST CALL
ZERO     DC    D'0.0'
ABENDFLG DS    X                  ONLINE ABEND FLAG
ABENDDEF EQU   X'01'              DEFAULT VALUE
EPNAME   DS    X                  ENTRY POINT IDENTIFICATION
TIMESTCL DC    X'00'              TIMEST CALL CHECK
CALLED   EQU   X'FF'              FLAG VALUE TO SHOW TIMEST CALLED
*
         LTORG                                                   170583
         NUCON                    CMS NUCLEUS DATA AREA LAYOUT
         BATLIMIT                 CMS BATCH DATA AREA LAYOUT
         END
#endif
