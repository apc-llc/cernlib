*
* $Id: csacom.F,v 1.1.1.1 1996/02/26 17:16:30 mclareni Exp $
*
* $Log: csacom.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:30  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.17/07 14/12/93  11.21.06  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   14/12/93
      INTEGER FUNCTION CSACOM(NM)
***----------------------------------
*  csacom returns  index of common block record
***----------------------------------
         INTEGER CSLTGB
         CHARACTER*(*) NM
      CALL CSCHID(NM)
      CSACOM=CSLTGB(IPVS)
      END
