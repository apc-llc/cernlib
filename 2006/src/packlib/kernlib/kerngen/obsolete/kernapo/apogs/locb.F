*
* $Id: locb.F,v 1.1.1.1 1996/02/15 17:51:03 mclareni Exp $
*
* $Log: locb.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:03  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION LOCB (AM)
C
C CERN PROGLIB# N100    LOCB            .VERSION KERNAPO  1.19  891207
C ORIG. 11/11/89  JZ
C
      DIMENSION AM(9)

      LOCB= IADDR(AM(1))
      RETURN
      END
#ifdef CERNLIB_CCGEN_LOCB
#undef CERNLIB_CCGEN_LOCB
#endif
#ifdef CERNLIB_TCGEN_LOCB
#undef CERNLIB_TCGEN_LOCB
#endif
