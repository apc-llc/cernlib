*
* $Id: near1.F,v 1.1.1.1 1996/04/01 15:03:12 mclareni Exp $
*
* $Log: near1.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:12  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      FUNCTION NEAR1(X)
      I=X
      XI=I
      XX=X-XI
      IF(XX.GE.0.5) GO TO 1
      NEAR1=I
      RETURN
    1 NEAR1=I+1
      RETURN
      END
