*
* $Id: mbytet.F,v 1.1.1.1 1996/02/15 17:52:37 mclareni Exp $
*
* $Log: mbytet.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:37  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      FUNCTION MBYTET (MZ,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    MBYTET          .VERSION KERNCRY  1.06  870527
C ORIG. 19/05/87  JZ
C

      MBYTET = IZW .AND. SHIFT (MZ.OR. MASK(64-NZB), IZP-1)
      RETURN
      END
#ifdef CERNLIB_TCGEN_MBYTET
#undef CERNLIB_TCGEN_MBYTET
#endif
