*
* $Id: dist.F,v 1.1.1.1 1996/02/15 17:53:25 mclareni Exp $
*
* $Log: dist.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:25  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION DIST(X,Y)
C
C
      DIMENSION X(3),Y(3)
C
      DIST=SQRT ((X(1)-Y(1))**2+(X(2)-Y(2))**2+(X(3)-Y(3))**2)
      RETURN
      END
