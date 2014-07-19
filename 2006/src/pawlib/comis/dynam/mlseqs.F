*
* $Id: mlseqs.F,v 1.1.1.1 1996/02/26 17:16:58 mclareni Exp $
*
* $Log: mlseqs.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:58  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if (defined(CERNLIB_IBM)||defined(CERNLIB_APOLLO)||!defined(CERNLIB_ASSEMB))&&(defined(CERNLIB_LIB))
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLSEQS(J1,N1,J2,N2)
      IF(N1.EQ.N2) GO TO 1
      MLSEQS=0
      RETURN
    1 MLSEQS=MLEQS(J1,J2,N2)
      RETURN
      END
#endif
