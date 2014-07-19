*
* $Id: dcimpy.F,v 1.1.1.1 1996/04/01 15:03:09 mclareni Exp $
*
* $Log: dcimpy.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:09  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE DCIMPY(D,Z1,Z)
      DOUBLE PRECISION D,Z1(2),Z(2),A
      A=-D*Z1(2)
      Z(2)=D*Z1(1)
      Z(1)=A
      RETURN
      END
