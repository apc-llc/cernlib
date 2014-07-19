*
* $Id: locf32.F,v 1.1.1.1 1996/02/15 17:52:22 mclareni Exp $
*
* $Log: locf32.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:22  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION LOCF (AM)
C
C CERN PROGLIB# N100    LOCF            .VERSION KERNCVX  1.09  910815
C ORIG. 01/01/89  AGA+MAV, Convex
C
      DIMENSION AM(9)

      LOCF= %LOC (AM(1)) / 4
      RETURN
      END
#ifdef CERNLIB_TCGEN_LOCF
#undef CERNLIB_TCGEN_LOCF
#endif
#ifdef CERNLIB_CCGEN_LOCF
#undef CERNLIB_CCGEN_LOCF
#endif
