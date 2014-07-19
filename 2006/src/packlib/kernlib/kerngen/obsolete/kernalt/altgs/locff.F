*
* $Id: locff.F,v 1.1.1.1 1996/02/15 17:50:55 mclareni Exp $
*
* $Log: locff.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:55  mclareni
* Kernlib
*
*
#include "kernalt/pilot.h"
      FUNCTION LOCF (AM)
C
C CERN PROGLIB# N100    LOCF            .VERSION KERNALT  1.01  880222
C ORIG. 22/02/88  JZ
C
      DIMENSION AM(9)
 
      LOCF= LOC (AM(1)) /4
      RETURN
      END
#ifdef CERNLIB_CCGEN_LOCF
#undef CERNLIB_CCGEN_LOCF
#endif
#ifdef CERNLIB_TCGEN_LOCF
#undef CERNLIB_TCGEN_LOCF
#endif
