*
* $Id: pspaw.F,v 1.3 1996/09/09 12:37:16 couet Exp $
*
* $Log: pspaw.F,v $
* Revision 1.3  1996/09/09 12:37:16  couet
* - pawcut.inc not used
*
* Revision 1.2  1996/04/24 09:39:11  dinofm
* Graphical cuts having more than 20 points are managed by getting many
* 'chunks' of points, each one consisting of max 20 points.
*
* Revision 1.1.1.1  1996/03/01 11:39:28  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/15 27/07/94  13.49.23  by  Fons Rademakers
*-- Author :    Fons Rademakers   07/10/93
      SUBROUTINE PSPAW(CHMESS,CHLINE,ISTAT)
************************************************************************
*                                                                      *
* Remote PAW procedure calls from the client to the master server or   *
* from a master server to the slave server.                            *
*                                                                      *
************************************************************************
*
#include "hbook/czbuff.inc"
#include "hbook/hcpiaf.inc"
#include "paw/quest.inc"
*
      PARAMETER (MAXBUFLEN = 480)
      CHARACTER*(*) CHMESS, CHLINE
      CHARACTER     CCHAIN*80, MEMBER*128
      CHARACTER*2048 TBUF
*
      ISTAT = 0
*
      IF (CHMESS .EQ. 'PACUTS') THEN
         READ(CHLINE,'(I4,I3,A67)') ID, NP, CHSMPF
         IF (NP .EQ. 2) THEN
            L = 24
         ELSE
            L = 24*NP
         ENDIF
*
         IF (L.GT.MAXBUFLEN) THEN
*-- The graphical cut has more than 20 points: Coordinates are split over
*-- multiple physical data buffer CHBUF.
            CALL CZGETC(MAXBUFLEN, ISTAT)
            TBUF = CHBUF (1:MAXBUFLEN)
            LPTR = MAXBUFLEN
            DO 111 LREM = L - MAXBUFLEN,0,-MAXBUFLEN
               LENCHUNK =  MIN(LREM,MAXBUFLEN)
               CALL CZGETC (LENCHUNK, ISTAT)
               TBUF = TBUF (1:LPTR) // CHBUF (1:LENCHUNK)
               LPTR = LPTR + LENCHUNK
 111        CONTINUE
         ELSE
            CALL CZGETC(L, ISTAT)
            TBUF = CHBUF (1:L)
         ENDIF

*-- Unpack the graphical cut structure from its string form to the native one
         CALL UNPACKGCUT(ID,CHSMPF,NP,TBUF,IERRP)
         IF (IERRP.EQ.0) THEN
*
*-- Propagate cut to active slave servers
*
            IF (MASTPF) THEN
               DO 10 I = 1, NSLAVE
                  CALL PFCUTS(ID, I, ISTAT)
  10           CONTINUE
            ENDIF
         ENDIF

      ELSEIF (CHMESS .EQ. 'PCHROP') THEN
         READ(CHLINE,'(I6)') L
         CALL CZGETC(L, ISTAT)
         READ(CHBUF,'(A80,A128)') CCHAIN, MEMBER
*
         CALL PCHROP(CCHAIN, MEMBER, IER)
*
*-- Propagate to active slave servers
*
         IF (MASTPF .AND. IER.EQ.0) THEN
            WRITE(CHSMPF,'(A,I6)') 'PCHROP', L
            DO 20 I = 1, NSLVPF
               CALL PFSOCK(I)
               CALL CZPUTA(CHSMPF, ISTAT)
               IF (ISTAT .NE. 0) GOTO 20
               CALL CZPUTC(L, ISTAT)
   20       CONTINUE
            CALL PSLOOP(NSLVPF, ISTAT)
         ENDIF
*
         IQUEST(1) = IER
         CALL PSPLOG(IQUEST(1), ISTAT)
*
      ENDIF
*
      END

