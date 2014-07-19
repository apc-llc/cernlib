*
* $Id: msbit.F,v 1.1.1.1 1996/02/15 17:52:37 mclareni Exp $
*
* $Log: msbit.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:37  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      FUNCTION MSBIT (MZ,IZW,IZP)
C
C CERN PROGLIB# M421    MSBIT           .VERSION KERNCRY  1.06  870527
C ORIG. 19/05/87  JZ
C

      MSBIT =     (IZW .AND. SHIFT(MASK(65),IZP))
     +        .OR. SHIFT(MZ.AND.MASK(127),IZP-1)
      RETURN
      END
#ifdef CERNLIB_TCGEN_MSBIT
#undef CERNLIB_TCGEN_MSBIT
#endif
