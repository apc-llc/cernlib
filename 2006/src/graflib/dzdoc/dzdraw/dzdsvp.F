*
* $Id: dzdsvp.F,v 1.1.1.1 1996/03/04 16:13:15 mclareni Exp $
*
* $Log: dzdsvp.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:15  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDSVP(IWID)
      REAL SMALER,DISPSZ(4)
      CALL IGQWK(IWID,'MXDS',DISPSZ)
      SMALER=MIN(DISPSZ(1),DISPSZ(2))
      CALL ISWKVP(IWID,0.,SMALER,0.,SMALER)
      END
************************************************************************
