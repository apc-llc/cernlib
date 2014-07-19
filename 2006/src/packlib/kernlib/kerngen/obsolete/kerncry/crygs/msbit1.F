*
* $Id: msbit1.F,v 1.1.1.1 1996/02/15 17:52:37 mclareni Exp $
*
* $Log: msbit1.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:37  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      FUNCTION MSBIT1 (IZW,IZP)
C
C CERN PROGLIB# M421    MSBIT1          .VERSION KERNCRY  1.06  870527
C ORIG. 19/05/87  JZ
C
      MSBIT1 =  IZW .OR.  SHIFT(MASK(1), IZP)
      RETURN
      END
#ifdef CERNLIB_TCGEN_MSBIT1
#undef CERNLIB_TCGEN_MSBIT1
#endif
