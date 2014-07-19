*
* $Id: dcsin.F,v 1.1.1.1 1996/04/01 15:03:11 mclareni Exp $
*
* $Log: dcsin.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:11  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE DCSIN(Z,F)
      DOUBLE PRECISION Z(2),F(2),A
      A=COS(Z(1))*SINH(Z(2))
      F(1)=SIN(Z(1))*COSH(Z(2))
      F(2)=A
      RETURN
      END
