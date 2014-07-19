*
* $Id: dimag.F,v 1.1.1.1 1996/04/01 15:03:08 mclareni Exp $
*
* $Log: dimag.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:08  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      DOUBLE PRECISION FUNCTION DIMAG(Z)
      DOUBLE PRECISION Z(2)
      DIMAG=Z(2)
      RETURN
      END
