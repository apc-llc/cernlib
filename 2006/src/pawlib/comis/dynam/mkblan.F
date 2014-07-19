*
* $Id: mkblan.F,v 1.1.1.1 1996/02/26 17:16:58 mclareni Exp $
*
* $Log: mkblan.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:58  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_IBM)||defined(CERNLIB_APOLLO)||!defined(CERNLIB_ASSEMB)
*CMZ :  1.18/00 17/02/94  16.48.18  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      FUNCTION MKBLAN(J,N)
#include "comis/csichv.inc"
      IF(N.LT.1) GO TO 4
      INVAR=J+N
      DO 2 LOOP=1,N
         ICHVAR=MKCHAR(J)
         IF(ICHVAR.NE.ICHBLN)GO TO 3
    1    J=J+1
    2 CONTINUE
    3 N=INVAR-J
      MKBLAN=ICHVAR
      RETURN
    4 MKBLAN=ICHBLN
      RETURN
      END
#endif
