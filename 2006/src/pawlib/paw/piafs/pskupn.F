*
* $Id: pskupn.F,v 1.2 1996/05/06 13:34:43 dinofm Exp $
*
* $Log: pskupn.F,v $
* Revision 1.2  1996/05/06 13:34:43  dinofm
* Code modified to take care of empty histograms detection on slave(s).
*
* Revision 1.1.1.1  1996/03/01 11:39:27  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/07 24/08/93  01.20.06  by  Fons Rademakers
*-- Author :    Fons Rademakers   16/08/93
      SUBROUTINE PSKUPN(CHCMD, NSLAV, ISTAT)
************************************************************************
*                                                                      *
* Send a Kuip command to NSLAV Piaf slave servers for execution.       *
* This routine is the master server equivalent of the client routine   *
* PFKUPN.                                                              *
*                                                                      *
************************************************************************
*
#include "hbook/czbuff.inc"
#include "hbook/hcpiaf.inc"
*
      CHARACTER*(*) CHCMD
      INTEGER EMASK
*
      ISTAT = 0
      CALL PFMASK(NSLAV, EMASK, -1)
      
*
*-- Check that there are slave servers running
*
      IF (NSLAV .EQ. 0) RETURN
*
*-- Multicast the command to the active slaves
*
      L = LENOCC(CHCMD)
      IF (L .LE. 74) THEN
         CHSMPF = 'KUIP :'//CHCMD
         CALL PSCAST(CHSMPF, NSLAV, ISTAT)
         IF (ISTAT .NE. 0) GOTO 90
      ELSE
         WRITE(CHSMPF,'(A,I6)') 'KUIP +', L
         CALL PSCAST(CHSMPF, NSLAV, ISTAT)
         IF (ISTAT .NE. 0) GOTO 90
         CHBUF(1:L) = CHCMD
         CALL PSCBUF(L, NSLAV, ISTAT)
         IF (ISTAT .NE. 0) GOTO 90
      ENDIF
*
*-- Clear HPLOPT and IGQ caches
*
      CALL PFHIGZ('CLEAR',' ',0,ISTAT)
      CALL PFPLOT('CLEAR',' ',NSLAV,0,ISTAT,EMASK)
      CALL PFPAW('CLEAR',' ',NSLAV,0,ISTAT,EMASK)
*
*-- Loop over requests from slave servers
*
      CALL PSLOOP(NSLAV, ISTAT, EMASK)
*
90    RETURN
      END
