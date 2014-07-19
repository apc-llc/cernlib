*
* $Id: dcmpy.F,v 1.1.1.1 1996/04/01 15:03:09 mclareni Exp $
*
* $Log: dcmpy.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:09  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE DCMPY(Z1,Z2,Z)
      DOUBLE PRECISION Z1(2),Z2(2),Z(2),A
      A=Z1(1)*Z2(1)-Z1(2)*Z2(2)
      Z(2)=Z1(2)*Z2(1)+Z1(1)*Z2(2)
      Z(1)=A
      RETURN
      END
