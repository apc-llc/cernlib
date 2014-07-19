*
* $Id: cktoiv.F,v 1.1.1.1 1996/02/26 17:16:22 mclareni Exp $
*
* $Log: cktoiv.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:22  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_PAW)
*CMZ :  1.11/01 24/01/91  11.36.50  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CKTOIV(LENGTH,INF,NDIM)
***--------------------------------------------
*       ku-info about vect converts to cs-info about array
***--------------------------------------------
      INTEGER LENGTH(3),INF(7)
      NDIM=1
      ICON=1
      N1=LENGTH(1)
      INF(5)=N1
      N2=LENGTH(2)
      IF(N2.GT.1)THEN
         NDIM=2
         ICON=N1+1
         INF(6)=N1*N2
         N3=LENGTH(3)
         IF(N3.GT.1)THEN
           NDIM=3
           ICON=ICON+INF(6)
           INF(7)=INF(6)*N3
         ENDIF
      ENDIF
      INF(1)=0
      INF(2)=0
      INF(3)=ICON
      INF(4)=NDIM
      END
#endif
