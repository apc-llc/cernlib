*
* $Id: jth10.F,v 1.1.1.1 1996/02/15 17:53:27 mclareni Exp $
*
* $Log: jth10.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:27  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION JTH10 (IWORD,J)
C
C
      NPOWER=10**(J-1)
      IF (J.EQ.1) NPOWER=1
      JTH10=MOD (IWORD/NPOWER,10)
      RETURN
      END
