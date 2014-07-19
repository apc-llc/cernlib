*
* $Id: locf.F,v 1.1.1.1 1996/02/15 17:52:37 mclareni Exp $
*
* $Log: locf.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:37  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      FUNCTION LOCF (AM)
C
C CERN PROGLIB# N100    LOCF            .VERSION KERNCRY  1.00  830612
C
      DIMENSION AM(9)

      LOCF= LOC (AM(1))
      RETURN
      END
#ifdef CERNLIB_CCGEN_LOCF
#undef CERNLIB_CCGEN_LOCF
#endif
#ifdef CERNLIB_TCGEN_LOCF
#undef CERNLIB_TCGEN_LOCF
#endif
