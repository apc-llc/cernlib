*
* $Id: c205d3.F,v 1.1.1.1 1996/02/15 17:48:41 mclareni Exp $
*
* $Log: c205d3.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:41  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
#if defined(CERNLIB_NUMLOPRE)
      FUNCTION C205D3(DX,I)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      C205D3=1
      RETURN
      ENTRY C205D4(DX,I)
      C205D4  =  SIN(1/DX)
      RETURN
      END
#endif
