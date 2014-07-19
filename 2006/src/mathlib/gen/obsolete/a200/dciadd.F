*
* $Id: dciadd.F,v 1.1.1.1 1996/04/01 15:03:09 mclareni Exp $
*
* $Log: dciadd.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:09  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE DCIADD(D,Z1,Z)
      DOUBLE PRECISION D,Z1(2),Z(2)
      Z(1)=Z1(1)
      Z(2)=D+Z1(2)
      RETURN
      END
