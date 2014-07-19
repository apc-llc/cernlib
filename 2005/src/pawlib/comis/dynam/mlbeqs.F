*
* $Id: mlbeqs.F,v 1.1.1.1 1996/02/26 17:16:58 mclareni Exp $
*
* $Log: mlbeqs.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:58  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLBEQS(J1,N1,J2,N2)
      MLBEQS=0
      IF((J1+N1) .LE. MJBGES(J1,N1,J2,N2)) MLBEQS=1
      RETURN
      END
