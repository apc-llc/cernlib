*
* $Id: bun8wcra.F,v 1.1.1.1 1996/03/08 15:21:54 mclareni Exp $
*
* $Log: bun8wcra.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:54  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_CRAY)
CDECK,BUN8WCRA,IF=CRAY,CONVEX.
      SUBROUTINE BUN8W(SOURCE,N1,TARGET,N2,N3)
C
C     BUNCHES (PACKS) 8 BIT WORDS (RIGHT ADJ., ZERO FILLED)
C             INTO CONTIGUOUS BIT STRING.
C
C  W.W. 17/04/84  SACLAY/DPHPE
C
      INTEGER SOURCE(2),TARGET(2)
C
#include "epio/paramcra.inc"
C
      IF(N3.LE.0)      RETURN
C
      NN1 = N1
      NN2 = (LBT8*N2-1)/NBW + 1
      NN3 = N3
      NPS = MOD(N2-1,NF8) + 1
C
      IF(NPS.EQ.1)     GO TO       21
C
      NPP = NF8 - NPS
C
      DO 11 I=NPP,0,-1
      CALL SBYT(SOURCE(NN1),TARGET(NN2),I*LBT8+1,LBT8)
      NN1 = NN1 + 1
      NN3 = NN3 - 1
      IF(NN3.EQ.0)   GO TO         999
 11   CONTINUE
C
      NN2 = NN2 + 1
 21   CALL BUNCH(SOURCE(NN1),TARGET(NN2),NN3,LBT8)
C
 999  RETURN
      END
#endif
