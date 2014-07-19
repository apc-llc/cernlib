*
* $Id: mcbyt.F,v 1.1.1.1 1996/02/15 17:52:37 mclareni Exp $
*
* $Log: mcbyt.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:37  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      FUNCTION MCBYT (MZ,IZQ,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    MCBYT           .VERSION KERNCRY  1.06  870527
C ORIG. 19/05/87  JZ
C

      MCBYT  =     (IZW.AND.SHIFT(MASK(64-NZB),IZP-1))
     +         .OR. SHIFT(SHIFTR(MZ,IZQ-1).AND.MASK(128-NZB),IZP-1)
      RETURN
      END
#ifdef CERNLIB_TCGEN_MCBYT
#undef CERNLIB_TCGEN_MCBYT
#endif
