*
* $Id: csltli.F,v 1.1.1.1 1996/02/26 17:16:17 mclareni Exp $
*
* $Log: csltli.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:17  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 05/10/93  15.07.50  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSLTLI(IPVS)
***----------------------------
      INTEGER CSLTAB
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
      IP=IPLI
      I=CSLTAB(IP,IPVS,KSIDL-1)
      IF(I.NE.0)THEN
        CALL CCOPYA(IQ(I+2),ISHGI,KSIDL-2)
      ELSE
*        CALL VFILL(ISHGI,KSIDL-2,0)
        CALL VZERO(ISHGI,KSIDL-2)
      ENDIF
      CSLTLI=I
      END
