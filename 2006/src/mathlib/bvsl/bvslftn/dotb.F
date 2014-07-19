*
* $Id: dotb.F,v 1.1.1.1 1996/03/21 17:19:55 mclareni Exp $
*
* $Log: dotb.F,v $
* Revision 1.1.1.1  1996/03/21 17:19:55  mclareni
* Bvsl
*
*
#include "pilot.h"
*CMZ :          06/11/90  14.36.25  by  Michel Roethlisberger/IBM
*-- Author :
      FUNCTION DOTB (NW,LV,Y,X)
      LOGICAL    LV(*)
      DIMENSION  Y (*), X(*)
      DOTB=0.
      DO 1 I = 1,NW
        IF(LV(I)) DOTB=DOTB+Y(I)*X(I)
    1 CONTINUE
      RETURN
      END
