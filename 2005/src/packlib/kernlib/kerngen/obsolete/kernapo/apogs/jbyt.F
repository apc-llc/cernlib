*
* $Id: jbyt.F,v 1.1.1.1 1996/02/15 17:51:02 mclareni Exp $
*
* $Log: jbyt.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:02  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION JBYT (IZW,IZP,NZB)
C
C CERN PROGLIB# M421    JBYT            .VERSION KERNAPO  1.08  860429
C ORIG. 12/12/83  JZ
C
      JBYT = RSHFT (LSHFT(IZW,33-IZP-NZB),32-NZB)
      RETURN
      END
#ifdef CERNLIB_TCGEN_JBYT
#undef CERNLIB_TCGEN_JBYT
#endif
