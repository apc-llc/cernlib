*
* $Id: jbytor.F,v 1.1.1.1 1996/02/15 17:52:37 mclareni Exp $
*
* $Log: jbytor.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:37  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      FUNCTION JBYTOR (MZ,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    JBYTOR          .VERSION KERNCRY  1.06  870527
C ORIG. 19/05/87  JZ
C
      JBYTOR = MZ .OR. (SHIFTR(IZW,IZP-1) .AND. MASK(128-NZB))
      RETURN
      END
#ifdef CERNLIB_TCGEN_JBYTOR
#undef CERNLIB_TCGEN_JBYTOR
#endif
