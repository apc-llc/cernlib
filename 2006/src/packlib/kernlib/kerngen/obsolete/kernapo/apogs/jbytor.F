*
* $Id: jbytor.F,v 1.1.1.1 1996/02/15 17:51:03 mclareni Exp $
*
* $Log: jbytor.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:03  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION JBYTOR (MZ,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    JBYTOR          .VERSION KERNAPO  1.10  870524
C ORIG. 19/05/87  JZ
C
      JBYTOR = OR (MZ, RSHFT (LSHFT(IZW,33-IZP-NZB),32-NZB) )
      RETURN
      END
#ifdef CERNLIB_TCGEN_JBYTOR
#undef CERNLIB_TCGEN_JBYTOR
#endif
