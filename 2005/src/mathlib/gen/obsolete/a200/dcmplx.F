*
* $Id: dcmplx.F,v 1.1.1.1 1996/04/01 15:03:08 mclareni Exp $
*
* $Log: dcmplx.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:08  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE DCMPLX(X,Y,Z)
      DOUBLE PRECISION X,Y,Z(2)
      Z(1)=X
      Z(2)=Y
      RETURN
      END
