*
* $Id: pfcuts.F,v 1.4 1996/09/09 09:28:22 couet Exp $
*
* $Log: pfcuts.F,v $
* Revision 1.4  1996/09/09 09:28:22  couet
* - pawcuts.inc not used
*
* Revision 1.3  1996/03/29 16:02:35  cernlib
* Replace TAB by blanks; cpp replaces a TAB by a single blank!
*
* Revision 1.2  1996/03/15 10:29:33  dinofm
* RLOGIN subroutine has been modified so that once the connection to PIAF is up,
* the PIAF master is queried about his version using the PIAF MESSAGE command
* back door.
*
* Revision 1.1.1.1  1996/03/01 11:38:45  mclareni
* Paw
*
*
#include "paw/pilot.h"
#if defined(CERNLIB_CZ)
*CMZ :  2.05/08 10/06/94  15.18.45  by  Fons Rademakers
*-- Author :    Fons Rademakers   07/10/93
      SUBROUTINE PFCUTS(ID,ISLAV,ISTAT)
*
* Send a graphical cut definition to the Piaf master server or a slave server.
*
      PARAMETER (MAXBUFLEN = 480)
#include "hbook/czbuff.inc"
#include "hbook/hcpiaf.inc"
      CHARACTER*2048 TBUF

*
*--- check that Piaf server is connected
*
      IF (CONNPF) THEN
         CALL PFSOCK(ISLAV)
*
*-- packs a graphical cut
*
         CALL PACKGCUT (ID,CHSMPF,L,TBUF,ISTAT)
         IF (ISTAT .EQ. 0) THEN
*-- Send the PACUTS pseudo-command 
            CALL CZPUTA(CHSMPF,ISTAT)
         ENDIF
         IF (ISTAT .NE. 0) GOTO 999
*
*-- send the coordinates
*
         IF (L.GT.480) THEN
*-- The graphical cut has more than 20 points but we are dealing with a new
*-- PIAF server that can cope with this. The TBUF data buffer is split in
*-- chunks of size MAXLENBUF then sent on the physical data buffer CHBUF.
            CHBUF = TBUF (:MAXBUFLEN)
*-- Send 1st chunk
            CALL CZPUTC(MAXBUFLEN, ISTAT)
            IF(ISTAT .NE. 0) GOTO 999
            LPTR = MAXBUFLEN
            DO 111 LREM = L - MAXBUFLEN,0,-MAXBUFLEN
               LENCHUNK =  MIN(LREM,MAXBUFLEN)
               CHBUF = TBUF (LPTR + 1 : LPTR + LENCHUNK)
               LPTR = LPTR + LENCHUNK
*-- Send further chunks
               CALL CZPUTC (LENCHUNK, ISTAT)
               IF(ISTAT .NE. 0) GOTO 999
 111            CONTINUE
         ELSE
*-- Less or equal than 20 points
            CHBUF = TBUF (:L)
            CALL CZPUTC(L, ISTAT)
         ENDIF
      ELSE
         PRINT *,'No connection to Piaf server'
         ISTAT=1
         RETURN
      ENDIF
*
 999  CONTINUE
*
*-- Reset ISKIN and ISKOUT to talk again from master to client
*
      CALL PFSOCK(0)
*
      END
#endif
