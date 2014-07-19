*
* $Id: pslvio.F,v 1.1.1.1 1996/03/01 11:39:28 mclareni Exp $
*
* $Log: pslvio.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:28  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/08 09/06/94  18.58.54  by  Fons Rademakers
*-- Author :    Fons Rademakers   18/08/93
      SUBROUTINE PSLVIO(NSLAV, ISTAT)
************************************************************************
*                                                                      *
* Get the real and virt. memory IO statistics from the slave servers.  *
*                                                                      *
************************************************************************
*
#include "hbook/hcpiaf.inc"
*
      SRIN   = 0
      SROUT  = 0
      SVMIO  = 0
      STUSER = 0
      STCPU  = 0
      ISTAT  = 0
      IF (NSLAV .EQ. 0) RETURN
*
*-- Ask I/O and CPU status from slaves
*
      CALL PSCAST('IOSTAT', NSLAV, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL PIAFLG(2,'Error broadcasting IOSTAT')
         GOTO 20
      ENDIF
*
*-- Loop over slave server replies in the order they are coming in
*
      CALL PFMASK(NSLAV, SMASK, -1)
10    CALL PSELCT(NSLAV, SMASK, ISLAV)
      IF (ISLAV .LT. 0) THEN
         ISTAT = 1
         CALL PIAFLG(2,'Slaves do not respond to IOSTAT')
         GOTO 20
      ENDIF
*
*-- Get slaves real and virtual IO statistics
*
      CALL PFSOCK(ISLAV)
      CALL CZGETA(CHSMPF, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL PIAFLG(2,'Error getting I/O statistics')
         GOTO 15
      ENDIF
*
      READ(CHSMPF,'(3F16.0,2F16.3)') RIN, ROUT, VIO, USER, CPU
      SRIN   = SRIN   + RIN
      SROUT  = SROUT  + ROUT
      SVMIO  = SVMIO  + VIO
      STUSER = STUSER + USER
      STCPU  = STCPU  + CPU
*
15    CALL PFMASK(ISLAV, SMASK, 0)
      IF (SMASK .EQ. 0) GOTO 20
      GOTO 10
*
20    CALL PFSOCK(0)
*
**    WRITE(CHSMPF, '(A,2F16.0,I2)') 'PSLVIO:', SRIN, SROUT, NSLAV
**    CALL PIAFLG(1,CHSMPF(1:LENOCC(CHSMPF)))
*
      END
