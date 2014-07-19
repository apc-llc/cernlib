*
* $Id: jbyt.F,v 1.1.1.1 1996/02/15 17:52:36 mclareni Exp $
*
* $Log: jbyt.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:36  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      FUNCTION JBYT (IZW,IZP,NZB)
C
C CERN PROGLIB# M421    JBYT            .VERSION KERNCRY  1.16  911111
C ORIG. 24/10/91  JZ
C
      JBYT = SHIFTR(IZW,IZP-1) .AND. MASK(128-NZB)
      RETURN
      END
#ifdef CERNLIB_TCGEN_JBYT
#undef CERNLIB_TCGEN_JBYT
#endif
