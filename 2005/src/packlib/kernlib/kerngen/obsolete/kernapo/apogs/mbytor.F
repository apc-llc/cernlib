*
* $Id: mbytor.F,v 1.1.1.1 1996/02/15 17:51:03 mclareni Exp $
*
* $Log: mbytor.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:03  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION MBYTOR (MZ,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    MBYTOR          .VERSION KERNAPO  1.10  870524
C ORIG. 19/05/87  JZ
C

      MBYTOR = OR (IZW, RSHFT (LSHFT(MZ,32-NZB), 33-IZP-NZB) )
      RETURN
      END
#ifdef CERNLIB_TCGEN_MBYTOR
#undef CERNLIB_TCGEN_MBYTOR
#endif
