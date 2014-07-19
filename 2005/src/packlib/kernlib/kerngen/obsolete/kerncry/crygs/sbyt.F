*
* $Id: sbyt.F,v 1.1.1.1 1996/02/15 17:52:38 mclareni Exp $
*
* $Log: sbyt.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:38  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      SUBROUTINE SBYT (MZ,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    SBYT            .VERSION KERNCRY  1.16  911111
C ORG. 24/10/91, JZ
C
      IZW = (IZW .AND. SHIFT(MASK(64-NZB),IZP-1))
     +            .OR. SHIFT(MZ.AND.MASK(128-NZB),IZP-1)
      RETURN
      END
#ifdef CERNLIB_TCGEN_SBYT
#undef CERNLIB_TCGEN_SBYT
#endif
