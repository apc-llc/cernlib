*
* $Id: cdex_db.F,v 1.1.1.1 1996/02/28 16:23:39 mclareni Exp $
*
* $Log: cdex_db.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:39  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBLISF(CMND,NENT)
*     ----------------------------
*                        -- Author :    Boris Khomenko   10/02/94
      IMPLICIT NONE
*
      CHARACTER CMND*(*)
      INTEGER   NENT
*
#include "cdxfils.inc"
*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC+KEEP,CDXFILS.
CC*
CC      INTEGER MCDXFIL
CC      PARAMETER (MCDXFIL = 10)
CC      COMMON /CDXFILS/ NCDXFIL,KCDXFIL,
CC     +                 ICXFSTA(10),CDXFPRF(10),CDXFTOP(10),CDXFNAM(10)
CC      INTEGER   NCDXFIL, KCDXFIL, ICXFSTA
CC      CHARACTER CDXFPRF*2, CDXFTOP*8, CDXFNAM*40
CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*
*     -- externals --
      INTEGER   LNBLNK
*
*     -- local variables --
      CHARACTER MESL*78
      INTEGER   LMES
*
      INTEGER   K , LL , LR
*
      MESL=' '
      LL=MAX0(LNBLNK(CMND),1)
      LL=LL+4
      MESL(3:LL)=CMND//'>>'
      DO 111 K=0,NCDXFIL
        CALL UXFILLN(0,MESL(LL+2:),LR)
        LL=LL+LR
  111 CONTINUE
      END
