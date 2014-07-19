*
* $Id: dcabs.F,v 1.1.1.1 1996/04/01 15:03:08 mclareni Exp $
*
* $Log: dcabs.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:08  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      DOUBLE PRECISION FUNCTION DCABS(Z)
      DOUBLE PRECISION Z(2)
      DCABS=SQRT(Z(1)**2+Z(2)**2)
      RETURN
      END
