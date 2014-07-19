*
* $Id: cssetc.F,v 1.1.1.1 1996/02/26 17:16:58 mclareni Exp $
*
* $Log: cssetc.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:58  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_IBM)||defined(CERNLIB_APOLLO)||!defined(CERNLIB_ASSEMB)
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSSETC(J,N,K)
      IF(N.LT.1)RETURN
      DO 1 LOOP=1,N
         CALL CSCHAR(J+LOOP-1,K)
    1 CONTINUE
    2 RETURN
      END
#endif
