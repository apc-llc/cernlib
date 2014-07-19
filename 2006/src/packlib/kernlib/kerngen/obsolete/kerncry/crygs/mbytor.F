*
* $Id: mbytor.F,v 1.1.1.1 1996/02/15 17:52:37 mclareni Exp $
*
* $Log: mbytor.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:37  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      FUNCTION MBYTOR (MZ,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    MBYTOR          .VERSION KERNCRY  1.06  870527
C ORIG. 19/05/87  JZ
C

      MBYTOR = IZW .OR. SHIFT (MZ.AND.MASK(128-NZB),IZP-1)
      RETURN
      END
#ifdef CERNLIB_TCGEN_MBYTOR
#undef CERNLIB_TCGEN_MBYTOR
#endif
