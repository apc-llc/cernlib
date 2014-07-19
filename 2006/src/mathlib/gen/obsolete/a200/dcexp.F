*
* $Id: dcexp.F,v 1.1.1.1 1996/04/01 15:03:10 mclareni Exp $
*
* $Log: dcexp.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:10  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE DEXP(Z,F)
      DOUBLE PRECISION Z(2),F(2),A
      A=EXP(Z(1))
      F(1)=A*COS(Z(2))
      F(2)=A*SIN(Z(2))
      RETURN
      END
