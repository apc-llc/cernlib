*
* $Id: jobstp.s,v 1.1.1.1 1996/02/15 17:51:56 mclareni Exp $
*
* $Log: jobstp.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:56  mclareni
* Kernlib
*
*
#if defined(CERNLIB_CERN)
         PRINT NOGEN
JOBSTP   CSECT
#if defined(CERNLIB_QMIBMXA)
JOBSTP   AMODE ANY
JOBSTP   RMODE ANY
#endif
*
* CERN PROGLIB#         JOBSTP          .VERSION KERNIBM  2.11  850320
* ORIG. 01/03/85  HRR
*
*
* THIS FORTRAN CALLABLE FUNCTION SUBROUTINE CAN BE USED TO TELL
* IF JOBSTOP IS NOW SET IN THE VIRTUAL MACHINE. THIS IS A MORE
* EFFICIENT METHOD FOR DOING THIS THAN CALLING THE JOBSTOP EXEC
* USING KMAND.
*
* FORTRAN CALLING SEQUENCE:  I=JOBSTP(N)
*
* WHERE: N IS IGNORED
*
*        I = INTEGER*4
*            0, JOB IS NOT REQUESTED TO STOP.
*            4, JOB IS REQUESTED TO STOP.
*
* DJOHNSON 05/02/84
*
         USING JOBSTP,R15               TEMPORARY ADDRESSABILITY
         B     JOBSTP0                  BYPASS IDENTIFICATION
         DC    XL1'7'                   ...
         DC    CL7'JOBSTP'              FOR STANDARD REPORTS
JOBSTP0  STM   R14,R12,12(R13)          SAVE CALLING REGISTERS
         LR    R12,R15                  BASE ADDRESS IN R12
         DROP  R15                      DROP TEMPORARY BASE
         USING JOBSTP,R12               PERMANENT BASE
         SR    R4,R4                    SET DEFAULT RETCODE
         ST    R4,RETCODE               ...
*--------------------------------------------------------------------
* - ISSUE CP VARIABLE QUERY JOBSTOP_COMMAND, AND LOOK AT RESPONSE
*--------------------------------------------------------------------
         LA    R4,CPCOM                 ADDRESS OF CP COMMAND
         LA    R6,L'CPCOM               LENGTH OF COMMAND
         ICM   R6,B'1000',=X'40'        WANT ANSWER IN BUFFER
         LA    R5,BUF                   RESPONSE BUFFER
         LA    R7,L'BUF                 LENGTH OF RESPONSE BUFFER
         DC    X'83460008'              CP CONSOLE FUNCTION DIAGNOSE
         CLC   YES,BUF                  ARE WE STOPPING?
         BNE   EXIT                     NO, RC=0
         MVC   RETCODE,=F'4'            YES, RC=4
*--------------------------------------------------------------------
* - RESTORE REGISTERS AND RETURN TO CALLER
*--------------------------------------------------------------------
EXIT     L     R14,12(,R13)             RESTORE REGISTER 14
         L     R0,RETCODE               GIVE BACK FUNCTION VALUE
         LM    R2,R12,28(R13)           RESTORE REGISTERS 2 THRU 12
         MVI   12(R13),X'FF'            INDICATE CONTROL RETURNED
         SR    R15,R15                  CLEAR R15
         BR    R14                      RETURN TO CALLER
*
*--------------------------------------------------------------------
* - DATA AREAS
*--------------------------------------------------------------------
RETCODE  DC    F'0'                     0 OR 4
YES      DC    C'JOBSTOP_COMMAND NOW '  YES ANSWER
BUF      DS    CL256                    CP COMMAND RESPONSE
CPCOM    DC    C'GVAR QUERY JOBSTOP_COMMAND' STOPPING?
         REGEQU
         END
#endif
