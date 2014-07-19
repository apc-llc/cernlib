*
* $Id: psloop.F,v 1.2 1996/05/06 13:34:45 dinofm Exp $
*
* $Log: psloop.F,v $
* Revision 1.2  1996/05/06 13:34:45  dinofm
* Code modified to take care of empty histograms detection on slave(s).
*
* Revision 1.1.1.1  1996/03/01 11:39:27  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/13 18/02/94  17.38.08  by  Fons Rademakers
*-- Author :    Fons Rademakers   16/08/93
      SUBROUTINE PSLOOP(NSLAV, ISTAT, EMASK)
************************************************************************
*                                                                      *
* Loop over requests from the slave servers.                           *
* This is the master server equivalent of the client PFLOOP routine.   *
*                                                                      *
************************************************************************
*
#include "hbook/hcpiaf.inc"
#include "paw/pccmd.inc"
*
      CHARACTER   CHMESS*6, CHLINE*74
      INTEGER EMASK
*
      IERROR = 0
      ISTAT  = 0
      IF (NSLAV .EQ. 0) RETURN
*
*-- Loop over slave server replies and requests
*
      CALL PFMASK(NSLAV, SMASK, -1)
10    CALL PSELCT(NSLAV, SMASK, ISLAV)
      IF (ISLAV .LT. 0) THEN
         ISTAT = 1
         GOTO 90
      ENDIF
      CALL PFSOCK(ISLAV)
      CALL CZGETA(CHSMPF, ISTAT)
      IF (ISTAT .NE. 0) GOTO 90
      CALL PFLOG(8,'PSLOOP',CHSMPF)
*
      CHMESS = CHSMPF(:6)
      CHLINE = CHSMPF(7:)
*
      IF (CHMESS(:1) .EQ. 'I') THEN
*
*-- Remote HIGZ
*
         CALL PFHIGZ(CHMESS,CHLINE,ISLAV,ISTAT)
*
      ELSEIF (CHMESS(:2) .EQ. 'HP') THEN
*
*-- Remote HPLOT
*
         CALL PFPLOT(CHMESS,CHLINE,NSLAV,ISLAV,ISTAT,EMASK)
*
      ELSEIF (CHMESS(:1) .EQ. 'P') THEN
*
*-- Remote PAW
*
         CALL PFPAW(CHMESS,CHLINE,NSLAV,ISLAV,ISTAT,EMASK)
*
      ELSEIF (CHMESS .EQ. 'HEXIST') THEN
*
*-- Does histogram exist?
*
         READ(CHLINE,'(I12)') IDH
         CALL PFSOCK(0)
         CALL HIDPOS(IDH, IDPOS)
         CALL PFSOCK(ISLAV)
         IF (IDPOS .GT. 0) THEN
            CALL CZPUTA('YES',ISTAT)
         ELSE
            CALL CZPUTA('NO',ISTAT)
         ENDIF
*
      ELSEIF (CHMESS .EQ. 'HLOGPF') THEN
*
*-- Import log file (i.e. end of KUIP command)
*
         CALL HLOGPF(CHSMPF, ISTAT)
         IF (NTCMD) CALL PNTLOG(6, 0, 0.)
*-- FIXME: Probably should save the QUEST vector where IQUEST(1).NE.0
         CALL PFMASK(ISLAV, SMASK, 0)
         IF (SMASK .EQ. 0) GOTO 20
         GOTO 10
*
      ELSEIF (CHMESS .EQ. 'HINPF') THEN
*
*-- Import histogram and add to already existing one
*
         READ(CHLINE,'(I12)') IDH
         CALL HINPF(IDH,1)
*
      ELSEIF (CHMESS .EQ. 'HOUTPF') THEN
*
*-- Export histogram
*
         READ(CHLINE,'(I12)') IDH
         CALL HOUTPF(IDH)
*
      ELSEIF (CHMESS .EQ. 'KISNDV') THEN
*
*-- Export a vector if it exists
*
         CALL PFSOCK(0)
         CALL KUVECT(CHLINE, LL, LH)
         CALL PFSOCK(ISLAV)
         IF (LL .EQ. 0) THEN
*-- can not just call KISNDV because that calls again KUVECT which in turn
*-- would ask the unknown vector from the slave (deadlock)
            CHSMPF = ' UNKNOWN VECTOR '//CHLINE
            CALL CZPUTA(CHSMPF,ISTAT)
         ELSE
            CALL KISNDV(CHLINE, ISTAT)
         ENDIF
*
      ELSEIF (CHMESS .EQ. 'BREAK*') THEN
*
*-- Slave had an exception
*
         CALL CZGETA(CHSMPF, ISTAT)
         PRINT '(A)', CHLINE
         CALL HLOGPF(CHSMPF, ISTAT)
         IERROR = 1
*
*-- Send a hard interrupt to all slaves except the one that generated
*-- the exception
*
         CALL PFMASK(NSLAV, SMASK, -1)
         CALL PFMASK(ISLAV, SMASK, 0)
         CALL PSINTR(SMASK)
         GOTO 90
*
      ENDIF
*
      GOTO 10
*
20    CONTINUE
*
90    CALL PFSOCK(0)
*
      IF (IERROR .NE. 0) ISTAT = 1
*
      END
