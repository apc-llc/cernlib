*
* $Id: mjsins.F,v 1.1.1.1 1996/02/26 17:16:58 mclareni Exp $
*
* $Log: mjsins.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:58  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_IBM)||defined(CERNLIB_APOLLO)||!defined(CERNLIB_ASSEMB)
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MJSINS(J1,N1,J2,N2)
      N=N2-N1+1
      IF(N1.LT.1) GO TO 4
      IF(N2.LT.1) GO TO 3
      IF(N.LT.1) GO TO 3
      DO 1 LOOP=1,N
         JVAR=J2+LOOP-1
         IF(MLEQS(J1,JVAR,N1)) 1,1,2
    1 CONTINUE
      MJSINS=0
      RETURN
    2 MJSINS=JVAR
      RETURN
    3 MJSINS=0
      RETURN
    4 MJSINS=J2
      RETURN
      END
#endif
