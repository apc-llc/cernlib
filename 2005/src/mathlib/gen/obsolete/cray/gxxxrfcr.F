*
* $Id: gxxxrfcr.F,v 1.1.1.1 1996/04/01 15:03:23 mclareni Exp $
*
* $Log: gxxxrfcr.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:23  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
#if defined(CERNLIB_CRAY)
      REAL FUNCTION GXXXRF()
      REAL GXXXGT,GXXXST,DS(2),DM(2),SEED,DX24,DX48,DL,DC,DU
      DATA DS   /1665 1885D0, 286 8876D0/
      DATA DM   /1518 4245D0, 265 1554D0/
      DATA DX24 /1677 7216D0/
      DATA DX48 /281 4749 7671 0656D0/
      DL=DS(1)*DM(1)
      DC=AINT(DL/DX24)
      DL=DL-DC*DX24
      DU=DS(1)*DM(2)+DS(2)*DM(1)+DC
      DS(2)=DU-AINT(DU/DX24)*DX24
      DS(1)=DL
      GXXXRF=(DS(2)*DX24+DS(1))/DX48
      RETURN
      ENTRY GXXXGT()
      GXXXGT=DS(2)*DX24+DS(1)
      RETURN
      ENTRY GXXXST(SEED)
      DS(2)=AINT(SEED/DX24)
      DS(1)=SEED-DS(2)*DX24
      GXXXST=DS(1)
      RETURN
      END
#endif
