*
* $Id: c205d1.F,v 1.1.1.1 1996/02/15 17:48:46 mclareni Exp $
*
* $Log: c205d1.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:46  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
      FUNCTION C205D1(DX,I)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      C205D1=(3*DX-1)*(1.5D0*DX-1)**4*EXP(-5*DX)
      RETURN
 
      ENTRY C205D2(DX,I)
      C205D2=SIN(DX)
      RETURN
      END
