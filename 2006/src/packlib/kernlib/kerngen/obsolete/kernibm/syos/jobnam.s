*
* $Id: jobnam.s,v 1.1.1.1 1996/02/15 17:53:21 mclareni Exp $
*
* $Log: jobnam.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:21  mclareni
* Kernlib
*
*
JOBNAM   CSECT
*
* CERN PROGLIB# Z100    JOBNAM          .VERSION KERNIBM  1.11  820328
*
*     CERN IBM 370/168 PROGRAM LIBRARY.                     13/09/77.
*     ASSEMBLER ROUTINE TO RETURN JOB NAME OF CALLING JOB.
*     AUTHOR : J.HART,RHEL.
*     SUBMITTER : H.RENSHALL,CERN.
*     CALLED FROM FORTRAN LIKE -
*
*        REAL*8 JBNAME
*        CALL JOBNAM (JBNAME)
*
*     THEN JBNAME IS THE 8 CHARACTER JOB NAME OF THE CALLING JOB.
*     NO EXTERNAL ROUTINES ARE REFERENCED.
*     JOBNAM SHOULD WORK ON ALL OS/360 AND OS/370 SYSTEMS.
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
JOBNAM   AMODE ANY
JOBNAM   RMODE ANY
#endif
         BC    15,12(,15)
         DC    X'7'
         DC    CL7'JOBNAM'
         STM   14,12,12(13)  SAVE CALLING PROGRAMS REGISTERS.
         BALR  12,0          LOAD BASE REGISTER.
         USING *,12
*     FIND JOB NAME FROM TIOT.
         L     2,16          LOAD CVT ADDRESS.
         L     3,0(,2)       LOAD TCB WORDS ADDRESS.
         L     4,4(,3)       LOAD TCB ADDRESS.
         L     5,12(,4)      LOAD TIOT ADDRESS.
         L     6,0(,1)       FETCH ARGUMENT ADDRESS.
         MVC   0(8,6),0(5)   MOVE 8 CHARACTER JOB NAME.
         LM    2,12,28(13)   RESTORE REGISTERS.
         MVI   12(13),X'FF'
         BR    14            RETURN
         END
