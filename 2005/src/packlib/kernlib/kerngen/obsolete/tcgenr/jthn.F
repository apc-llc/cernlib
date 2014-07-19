*
* $Id: jthn.F,v 1.1.1.1 1996/02/15 17:53:27 mclareni Exp $
*
* $Log: jthn.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:27  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION JTHN(IWORD,J,N)
C
C
      NPOWER = N**(J-1)
      IF(J.EQ.1) NPOWER=1
      JTHN = MOD(IWORD/NPOWER,N)
      RETURN
      END
