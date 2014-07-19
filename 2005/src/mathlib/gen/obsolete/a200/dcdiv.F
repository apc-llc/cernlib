*
* $Id: dcdiv.F,v 1.1.1.1 1996/04/01 15:03:09 mclareni Exp $
*
* $Log: dcdiv.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:09  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE DCDIV(Z1,Z2,Z)
      DOUBLE PRECISION Z1(2),Z2(2),Z(2),A,B
      A=1.0D0/(Z2(1)**2+Z2(2)**2)
      B=(Z1(1)*Z2(1)+Z1(2)*Z2(2))*A
      Z(2)=(Z1(2)*Z2(1)-Z1(1)*Z2(2))*A
      Z(1)=B
      RETURN
      END
