*
* $Id: dreal.F,v 1.1.1.1 1996/04/01 15:03:08 mclareni Exp $
*
* $Log: dreal.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:08  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      DOUBLE PRECISION FUNCTION DREAL(Z)
      DOUBLE PRECISION Z(2)
      DREAL=Z(1)
      RETURN
      END
