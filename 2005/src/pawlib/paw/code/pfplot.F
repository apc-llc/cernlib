*
* $Id: pfplot.F,v 1.3 1996/05/06 13:31:02 dinofm Exp $
*
* $Log: pfplot.F,v $
* Revision 1.3  1996/05/06 13:31:02  dinofm
* Code modified to take care of empty histograms detection on slave(s).
*
* Revision 1.2  1996/04/24 08:21:22  dinofm
* Handling of HPLFR3 remote HPLOT command.
*
* Revision 1.1.1.1  1996/03/01 11:38:46  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/09 15/12/93  15.19.50  by  Fons Rademakers
*-- Author :    Alfred Nathaniel   01/05/93
      SUBROUTINE PFPLOT(CHMESS,CHLINE,NSLAV,ISLAV,ISTAT,EMASK)
************************************************************************
*                                                                      *
* Remote HPLOT procedure calls from the master server to the client or *
* from a slave server to the master server. In case a slave calls the  *
* master, the master in turn makes an RPC to the client and, when      *
* necessary, relays the output parameter(s) back to the slave.         *
*                                                                      *
************************************************************************
*
#include "hbook/hcpiaf.inc"
#include "paw/quest.inc"
*
      CHARACTER*(*)  CHMESS, CHLINE
      CHARACTER      CHOPT*16, CHCASE*16
      CHARACTER*80   CHMAIL
      INTEGER        EMASK
*
*-- HPLOPT cache used on master server
*
      PARAMETER     (MAXOPT = 20)
      CHARACTER*4    HPOPT(MAXOPT)
      INTEGER        HPVAL(MAXOPT), NHPOPT, INIBW, INIF, HMASK
*
      SAVE           HPOPT, HPVAL, NHPOPT, INIBW, INIF, HMASK
*
      ISTAT = 0
*
      IF (CHMESS.EQ.'HPLOPT') THEN
*
         READ(CHLINE,'(I12,A)') N,CHOPT
*
*-- When on master check if option value is already in cache, if it is
*-- don't go to client
*
         IF (MASTPF .AND. N.LT.0) THEN
            DO 10 I = 1, NHPOPT
               IF (CHOPT .EQ. HPOPT(I)) THEN
                  WRITE(CHCASE,'(I12)') HPVAL(I)
                  CALL CZPUTA(CHCASE,ISTAT)
                  RETURN
               ENDIF
   10       CONTINUE
         ENDIF
*
         IF (MASTPF) CALL PFSOCK(0)
         CALL HPLOPT(CHOPT,N)
         IF (N .LT. 0) THEN
            WRITE(CHCASE,'(I12)') IQUEST(11)
            IF (MASTPF) CALL PFSOCK(ISLAV)
            CALL CZPUTA(CHCASE,ISTAT)
*
*-- Fill cache
*
            IF (MASTPF .AND. NHPOPT.LT.MAXOPT) THEN
               NHPOPT = NHPOPT + 1
               HPOPT(NHPOPT) = CHOPT
               HPVAL(NHPOPT) = IQUEST(11)
            ENDIF
         ENDIF
*
      ELSEIF (CHMESS.EQ.'HPLOT') THEN
*
         READ(CHLINE,'(2I12,2A)') IDH,NUM,CHOPT,CHCASE
         IF (MASTPF) THEN
            CALL PFMASK(ISLAV, HMASK, 0)
            IF (IAND (HMASK,EMASK) .EQ. 0) THEN
               CALL PFSOCK(0)
               CALL HPLOT(IDH,CHOPT,CHCASE,NUM)
               IF (CHOPT.NE.'K' .AND. CHOPT.NE.'U') CALL HDELET(IDH)
               CALL PFMASK(NSLAV, HMASK, -1)
            ENDIF
         ELSE
            CALL HPLOT(IDH,CHOPT,CHCASE,NUM)
         ENDIF
*
      ELSEIF (CHMESS.EQ.'HPLSTA') THEN
*
         READ(CHLINE,'(2I12,A)') IDH,NUM,CHCASE
         IF (MASTPF) THEN
            CALL PFMASK(ISLAV, HMASK, 0)
            IF (IAND (HMASK,EMASK) .EQ. 0) THEN
               CALL PFSOCK(0)
               CALL HPLSTA(IDH,CHCASE,NUM)
               CALL HDELET(IDH)
               CALL PFMASK(NSLAV, HMASK, -1)
            ENDIF
         ELSE
            CALL HPLSTA(IDH,CHCASE,NUM)
         ENDIF
*
      ELSEIF (CHMESS.EQ.'HPRINT') THEN
*
         READ(CHLINE,'(I12)') IDH
         IF (MASTPF) THEN
            CALL PFMASK(ISLAV, HMASK, 0)
            IF (IAND (HMASK,EMASK) .EQ. 0) THEN
               CALL PFSOCK(0)
               CALL HPRINT(IDH)
               CALL HDELET(IDH)
               CALL PFMASK(NSLAV, HMASK, -1)
            ENDIF
         ELSE
            CALL HPRINT(IDH)
         ENDIF
*
      ELSEIF (CHMESS.EQ.'HPLTIT') THEN
*
         IF (MASTPF) CALL PFSOCK(0)
         CALL HPLTIT(CHLINE)
*
      ELSEIF (CHMESS.EQ.'HPLFR3') THEN
*
         CHOPT = CHLINE
         CALL CZGETA(CHMAIL,ISTAT)
         READ(CHMAIL,'(4E16.8)') X1I,X2I,Y1I,Y2I
         CALL CZGETA(CHMAIL,ISTAT)
         READ(CHMAIL,'(4E16.8)') Z1I,Z2I,THETA,PHI

*
* When master server, relay only the one call to HPLFR3 to the client
* (each slave would try to do the same)
         IF (MASTPF) THEN
            IF (CHOPT(1:2).EQ.'BW') THEN
               IF (INIBW .EQ. 0) THEN
                  INIBW = 1
                  CALL PFSOCK(0)
                  CALL HPLFR3(X1I,X2I,Y1I,Y2I,Z1I,Z2I,THETA,PHI,CHOPT)
               ENDIF
            ELSEIF (CHOPT(1:1).EQ.'F') THEN
               IF (INIF .EQ. 0) THEN
                  INIF = 1
                  CALL PFSOCK(0)
                  CALL HPLFR3(X1I,X2I,Y1I,Y2I,Z1I,Z2I,THETA,PHI,CHOPT)
               ENDIF
            ENDIF
         ELSE
            CALL HPLFR3(X1I,X2I,Y1I,Y2I,Z1I,Z2I,THETA,PHI,CHOPT)
         ENDIF

      ELSEIF (CHMESS.EQ.'CLEAR') THEN
*
*-- Clear HPLOPT cache
*
         NHPOPT = 0
         INIBW = 0
         INIF = 0
         CALL PFMASK(NSLAV, HMASK, -1)
*
      ELSE
         PRINT *,' PFPLOT:',CHMESS,'?? '
      ENDIF
*
      END









