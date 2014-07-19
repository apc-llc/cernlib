*
* $Id: locf.F,v 1.1.1.1 1996/02/15 17:51:03 mclareni Exp $
*
* $Log: locf.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:03  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION LOCF (AM)
C
C CERN PROGLIB# N100    LOCF            .VERSION KERNAPO  1.16  890321
C ORIG. 13/12/83  T. LINDELOF
C Mod.  14/02/89  JZ, to be usable also on DN 10000
C
      DIMENSION AM(9)

      LOCF= RSHFT (IADDR(AM(1)), 2)
      RETURN
      END
#ifdef CERNLIB_CCGEN_LOCF
#undef CERNLIB_CCGEN_LOCF
#endif
#ifdef CERNLIB_TCGEN_LOCF
#undef CERNLIB_TCGEN_LOCF
#endif
