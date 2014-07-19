*
* $Id: mnblan.F,v 1.1.1.1 1996/02/26 17:16:58 mclareni Exp $
*
* $Log: mnblan.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:58  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_IBM)||defined(CERNLIB_APOLLO)||!defined(CERNLIB_ASSEMB)
*CMZ :  1.18/00 17/02/94  16.50.06  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      FUNCTION MNBLAN(J,N)
#include "comis/csichv.inc"
      IF(N.LT.1) GO TO 3
      NVAR=N
      JMOD=J+N
      DO 1 LOOP=1,N
         IF(MKCHAR(JMOD-LOOP).NE.ICHBLN)GO TO 2
    1 NVAR=NVAR-1
    2 MNBLAN=NVAR
      RETURN
    3 MNBLAN=0
      RETURN
      END
#endif
