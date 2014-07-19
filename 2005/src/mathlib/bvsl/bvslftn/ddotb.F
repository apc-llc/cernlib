*
* $Id: ddotb.F,v 1.1.1.1 1996/03/21 17:19:55 mclareni Exp $
*
* $Log: ddotb.F,v $
* Revision 1.1.1.1  1996/03/21 17:19:55  mclareni
* Bvsl
*
*
#include "pilot.h"
*CMZ :          06/11/90  14.36.25  by  Michel Roethlisberger/IBM
*-- Author :
      FUNCTION DDOTB (NW,LV,Y,X)
      LOGICAL    LV(*)
#include "defsp.inc"
     +           Y (*), X(*), DDOTB
      DDOTB=0.
      DO 1 I = 1,NW
        IF(LV(I)) DDOTB=DDOTB+Y(I)*X(I)
    1 CONTINUE
      RETURN
      END
