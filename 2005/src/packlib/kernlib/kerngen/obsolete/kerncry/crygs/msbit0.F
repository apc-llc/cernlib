*
* $Id: msbit0.F,v 1.1.1.1 1996/02/15 17:52:37 mclareni Exp $
*
* $Log: msbit0.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:37  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      FUNCTION MSBIT0 (IZW,IZP)
C
C CERN PROGLIB# M421    MSBIT0          .VERSION KERNCRY  1.06  870527
C ORIG. 19/05/87  JZ
C
      MSBIT0 =  IZW .AND. SHIFT(MASK(65),IZP)
      RETURN
      END
#ifdef CERNLIB_TCGEN_MSBIT0
#undef CERNLIB_TCGEN_MSBIT0
#endif
