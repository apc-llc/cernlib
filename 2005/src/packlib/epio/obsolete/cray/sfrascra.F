*
* $Id: sfrascra.F,v 1.1.1.1 1996/03/08 15:21:55 mclareni Exp $
*
* $Log: sfrascra.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:55  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_CRAY)
CDECK,SFRASCRA,IF=CRAY,CONVEX
      SUBROUTINE SFRASC(SOURCE,N1,TARGET,N2,N3)
      CHARACTER SOURCE*(*),TARGET*(*)
C
C  W.W. 17/04/84  SACLAY/DPHPE
C
      IF(N3.LE.0)      RETURN
C
      TARGET(N2:N2+N3-1) = SOURCE(N1:N1+N3-1)
C
      RETURN
      END
#endif
