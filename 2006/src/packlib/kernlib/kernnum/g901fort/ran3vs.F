*
* $Id: ran3vs.F,v 1.1.1.1 1996/02/15 17:49:06 mclareni Exp $
*
* $Log: ran3vs.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:06  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE RAN3VS(RADIUS,X1,Y1,Z1)
  10      X  =  2.*RANF() - 1.
          Y  =  2.*RANF() - 1.
          Z  =  2.*RANF() - 1.
          RR =  X*X + Y*Y + Z*Z
          IF(RR .GT. 1.)  GOTO 10
          SCALE  =  RADIUS / SQRT(RR)
          X1  =  X * SCALE
          Y1  =  Y * SCALE
          Z1  =  Z * SCALE
          RETURN
          END
