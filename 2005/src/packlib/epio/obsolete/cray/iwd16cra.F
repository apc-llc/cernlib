*
* $Id: iwd16cra.F,v 1.1.1.1 1996/03/08 15:21:55 mclareni Exp $
*
* $Log: iwd16cra.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:55  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_CRAY)
CDECK,IWD16CRA,IF=CRAY,CONVEX
      FUNCTION IWD16(A,N)
C
C     FUNCTION RETURNS THE 16 BIT WORD NO. N  OF STRING  A.
C
C  W.W. 17/04/84  SACLAY/DPHPE
C
      DIMENSION A(2)
C
#include "epio/paramcra.inc"
C
      NN = (LBT16*N-1)/NBW + 1
      NS = MOD(N-1,NF16) + 1
      NP = NF16 - NS
C
      II = JBYT(A(NN),NP*LBT16+1,LBT16)
C
      IWD16 = II
C
      RETURN
      END
#endif
