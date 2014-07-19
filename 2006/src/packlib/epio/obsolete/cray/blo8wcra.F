*
* $Id: blo8wcra.F,v 1.1.1.1 1996/03/08 15:21:54 mclareni Exp $
*
* $Log: blo8wcra.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:54  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_CRAY)
CDECK,BLO8WCRA,IF=CRAY,CONVEX.
      SUBROUTINE BLO8W(SOURCE,N1,TARGET,N2,N3)
C
C     BLOWS (UNPACKS) 8 BIT WORDS INTO MACHINE WORDS.
C
C  W.W. 17/04/84  SACLAY/DPHPE
C
      INTEGER SOURCE(2),TARGET(2)
C
#include "epio/paramcra.inc"
C
      IF(N3.LE.0)      RETURN
C
      NN1 = (LBT8*N1-1)/NBW + 1
      NN2 = N2
      NN3 = N3
      NPS = MOD(N1-1,NF8) + 1
C
      IF(NPS.EQ.1)     GO TO       21
C
      NPP = NF8 - NPS
C
      DO 11 I=NPP,0,-1
      TARGET(NN2)=JBYT(SOURCE(NN1),I*LBT8+1,LBT8)
      NN2 = NN2 + 1
      NN3 = NN3 - 1
      IF(NN3.EQ.0)   GO TO         999
 11   CONTINUE
C
      NN1 = NN1 + 1
 21   CALL BLOW(SOURCE(NN1),TARGET(NN2),NN3,LBT8)
C
 999  RETURN
      END
#endif
