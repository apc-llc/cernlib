*
* $Id: pfkupn.F,v 1.1.1.1 1996/03/01 11:38:46 mclareni Exp $
*
* $Log: pfkupn.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:46  mclareni
* Paw
*
*
#include "paw/pilot.h"
#if defined(CERNLIB_CZ)
*CMZ :  2.03/16 07/10/93  16.51.22  by  Fons Rademakers
*-- Author :    Alfred Nathaniel   07/04/93
      SUBROUTINE PFKUPN(CHCMD,ISLAV,ISTAT)
      CHARACTER*(*) CHCMD
*
* Send a Kuip command to the Piaf master or a slave server for execution.
* Use only in sequential mode or when sending to one particular slave.
*
#include "hbook/czbuff.inc"
#include "hbook/hcpiaf.inc"
*
      CHARACTER CHMAIL*80
*
*--- check that Piaf server is connected
*
      IF (CONNPF) THEN
         CALL PFSOCK(ISLAV)
         L=LENOCC(CHCMD)
         IF(L.LE.74) THEN
            CHMAIL='KUIP :'//CHCMD
            CALL CZPUTA(CHMAIL,ISTAT)
            IF(ISTAT.NE.0) GOTO 999
         ELSE
            WRITE(CHMAIL,'(A,I6)') 'KUIP +',L
            CALL CZPUTA(CHMAIL,ISTAT)
            IF(ISTAT.NE.0) GOTO 999
            CHBUF(:L)=CHCMD
            CALL CZPUTC(L,ISTAT)
            IF(ISTAT.NE.0) GOTO 999
         ENDIF
         CALL PFLOOP(ISTAT)
         IF(ISTAT.NE.0) GOTO 999
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
