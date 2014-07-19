*
* $Id: cssetl.F,v 1.1.1.1 1996/02/26 17:16:16 mclareni Exp $
*
* $Log: cssetl.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:16  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.32  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSSETL(LPM,LFL,LLOG,LMAP,LEDT)
#include "comis/cslun.inc"
      SAVE NCALL
      DATA NCALL /0/
      IF(LPM.LE.0)THEN
         IF(NCALL.NE.0)RETURN
         LUNPM =11
         LUNFIL=12
         LUNLOG=13
         LUNMAP=14
         LUNLIB=15
      ELSE
         NCALL =1
         LUNPM =LPM
         LUNFIL=LFL
         LUNLOG=LLOG
         LUNMAP=LMAP
         LUNLIB=LEDT
      ENDIF
      END
