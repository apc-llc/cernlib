*
* $Id: mbytet.F,v 1.1.1.1 1996/02/15 17:51:03 mclareni Exp $
*
* $Log: mbytet.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:03  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION MBYTET (MZ,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    MBYTET          .VERSION KERNAPO  1.10  870524
C ORIG. 19/05/87  JZ
C

      MBYTET = AND (IZW,
     +              NOT(RSHFT (LSHFT(NOT(MZ),32-NZB), 33-IZP-NZB)) )
      RETURN
      END
#ifdef CERNLIB_TCGEN_MBYTET
#undef CERNLIB_TCGEN_MBYTET
#endif
