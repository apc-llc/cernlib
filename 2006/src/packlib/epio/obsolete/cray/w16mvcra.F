*
* $Id: w16mvcra.F,v 1.1.1.1 1996/03/08 15:21:55 mclareni Exp $
*
* $Log: w16mvcra.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:55  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_CRAY)
CDECK,W16MVCRA,IF=CRAY,CONVEX
      SUBROUTINE W16MOV(SOURCE,N1,TARGET,N2,N3)
      INTEGER SOURCE(2),TARGET(2)
C
C  W.W. 17/04/84  SACLAY/DPHPE
C
#include "epio/paramcra.inc"
C
      IF(N3.LE.0)      RETURN
C
      NN1 = (LBT16*N1 - 1) / NBW + 1
      NN2 = (LBT16*N2 - 1) / NBW + 1
      NN3 = N3
      NP1 = MOD(N1-1,NF16) + 1
      NP2 = MOD(N2-1,NF16) + 1
      NS1 = NF16 - NP1
      NS2 = NF16 - NP2
C
      DO 11 I = 1, NN3
      II = JBYT(SOURCE(NN1),NS1*LBT16+1,LBT16)
      CALL SBYT(II,TARGET(NN2),NS2*LBT16+1,LBT16)
      IF(NS1.EQ.0) NN1 = NN1 + 1
      IF(NS1.EQ.0) NS1 = NS1 + NF16
      IF(NS2.EQ.0) NN2 = NN2 + 1
      IF(NS2.EQ.0) NS2 = NS2 + NF16
      NS1 = NS1 - 1
      NS2 = NS2 - 1
 11   CONTINUE
C
      RETURN
      END
#endif
