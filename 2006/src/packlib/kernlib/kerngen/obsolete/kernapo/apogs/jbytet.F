*
* $Id: jbytet.F,v 1.1.1.1 1996/02/15 17:51:03 mclareni Exp $
*
* $Log: jbytet.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:03  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION JBYTET (MZ,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    JBYTET          .VERSION KERNAPO  1.08  860429
C ORIG. 12/12/83  JZ
C
      JBYTET = AND (MZ, RSHFT (LSHFT(IZW,33-IZP-NZB),32-NZB) )
      RETURN
      END
#ifdef CERNLIB_TCGEN_JBYTET
#undef CERNLIB_TCGEN_JBYTET
#endif
