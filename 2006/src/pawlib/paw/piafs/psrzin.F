*
* $Id: psrzin.F,v 1.1.1.1 1996/03/01 11:39:28 mclareni Exp $
*
* $Log: psrzin.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:28  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.02/01 07/05/93  00.22.52  by  Fons Rademakers
*-- Author :
      SUBROUTINE PSRZIN(CHLINE)
      CHARACTER*(*) CHLINE
*
*         read a record from a RZ file remotely
*
#include "hbook/hcbook.inc"
#include "paw/quest.inc"
      DIMENSION IQSAVE(100)
      CHARACTER*8 CHOPT
      INTEGER KEYS(2)
*
*
* Decode message from host
*
      READ(CHLINE,'(3I10,A)') KEYS(1),KEYS(2),ICYCLE,CHOPT
      IDH=KEYS(1)

*
*-- Don't look in memory because after a CD memory and directory
*-- don't correspond anymore. Also when using the S option KEYS(1)
*-- does not correspond to the IDH.
*
***      IF(IDH.NE.0 .AND. KEYS(2).EQ.0) THEN
*
* Look for histogram ID in memory
*
***         CALL HLOCAT(IDH,LCID)
***         IF(LCID.NE.0) THEN
***            IQSAVE(1)=0
***            IQSAVE(12)=IQ(LCID+KNTOT)
***            IQSAVE(14)=0
***            IQSAVE(40)=1
***            IQSAVE(41)=IDH
***            IQSAVE(42)=0
***            IF(INDEX(CHOPT,'D').NE.0) THEN
***               CALL FZOUT(998,IHDIV,LCID,1,' ',2,100,IQSAVE)
***            ELSE
***               CALL FZOUT(998,0,0,1,'Z',2,100,IQSAVE)
***            ENDIF
***            GOTO 99
***         ENDIF
***      ENDIF

*
* Read ZEBRA structure from RZ file
*
      LCID=0
      CALL RZIN(IHDIV,LCID,1,KEYS,ICYCLE,CHOPT)
      CALL UCOPY(IQUEST,IQSAVE,100)

*
* Transfer Data structure to client
*
      IF(LCID.NE.0)THEN
         CALL FZOUT(998,IHDIV,LCID,1,' ',2,100,IQSAVE)
         CALL MZDROP(IHDIV,LCID,' ')
         LCID=0
      ELSE
         CALL FZOUT(998,0,0,1,'Z',2,100,IQSAVE)
      ENDIF

 99   CONTINUE
      END
