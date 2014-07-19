*
* $Id: csltgp.F,v 1.1.1.1 1996/02/26 17:16:17 mclareni Exp $
*
* $Log: csltgp.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:17  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 05/10/93  15.05.11  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSLTGP(IPVS)
***----------------------------
      INTEGER CSLTAB
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
      IP=IPGP
      I=CSLTAB(IP,IPVS,KSIDP-1)
      IF(I.NE.0)THEN
        CALL CCOPYA(IQ(I+2),IADGP,KSIDP-2)
      ELSE
*        CALL VFILL(IADGP,KSIDP-2,0)
        CALL VZERO(IADGP,KSIDP-2)
      ENDIF
      CSLTGP=I
      END
