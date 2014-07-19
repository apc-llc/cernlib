*
* $Id: msbyt.F,v 1.1.1.1 1996/02/15 17:52:37 mclareni Exp $
*
* $Log: msbyt.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:37  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      FUNCTION MSBYT (MZ,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    MSBYT           .VERSION KERNCRY  1.06  870527
C ORIG. 19/05/87  JZ
C

      MSBYT =     (IZW .AND. SHIFT(MASK(64-NZB),IZP-1))
     +        .OR. SHIFT(MZ.AND.MASK(128-NZB),IZP-1)
      RETURN
      END
#ifdef CERNLIB_TCGEN_MSBYT
#undef CERNLIB_TCGEN_MSBYT
#endif
