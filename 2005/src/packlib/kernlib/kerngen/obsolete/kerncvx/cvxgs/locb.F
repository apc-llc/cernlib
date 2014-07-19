*
* $Id: locb.F,v 1.1.1.1 1996/02/15 17:52:27 mclareni Exp $
*
* $Log: locb.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:27  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION LOCB (AM)
C
C CERN PROGLIB# N100    LOCB            .VERSION KERNCVX  1.06  891207
C ORIG. 17/11/89  AGA+MAV, Convex
C
      DIMENSION AM(9)

      LOCB= %LOC (AM(1))
      RETURN
      END
#ifdef CERNLIB_TCGEN_LOCB
#undef CERNLIB_TCGEN_LOCB
#endif
#ifdef CERNLIB_CCGEN_LOCB
#undef CERNLIB_CCGEN_LOCB
#endif
