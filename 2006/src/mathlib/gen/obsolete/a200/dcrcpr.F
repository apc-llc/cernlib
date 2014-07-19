*
* $Id: dcrcpr.F,v 1.1.1.1 1996/04/01 15:03:09 mclareni Exp $
*
* $Log: dcrcpr.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:09  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE DCRCPR(Z1,Z)
      DOUBLE PRECISION Z1(2),Z(2),A
      A=1.0D0/(Z1(1)**2+Z1(2)**2)
      Z(1)=Z1(1)*A
      Z(2)=-Z1(2)*A
      RETURN
      END
