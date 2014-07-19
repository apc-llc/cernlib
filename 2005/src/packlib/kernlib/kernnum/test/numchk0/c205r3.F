*
* $Id: c205r3.F,v 1.1.1.1 1996/02/15 17:48:41 mclareni Exp $
*
* $Log: c205r3.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:41  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
      FUNCTION C205R3(RX,I)
      IMPLICIT REAL (A-H,O-Z)
      C205R3=1
      RETURN
      ENTRY C205R4(RX,I)
      C205R4  =  SIN(1/RX)
      RETURN
      END
