*
* $Id: mleqs.F,v 1.1.1.1 1996/02/26 17:16:58 mclareni Exp $
*
* $Log: mleqs.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:58  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_IBM)||defined(CERNLIB_APOLLO)||!defined(CERNLIB_ASSEMB)
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLEQS(J1,J2,N)
      IF(N.LT.1) GO TO 2
      DO 1 LOOP=1,N
         ICH1=MKCHAR(J1+LOOP-1)
         ICH2=MKCHAR(J2+LOOP-1)
         IF(ICH1.NE.ICH2) GO TO 3
    1 CONTINUE
    2 MLEQS=1
      RETURN
    3 MLEQS=0
      RETURN
      END
#endif
