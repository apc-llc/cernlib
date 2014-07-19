*
* $Id: msbit1.F,v 1.1.1.1 1996/02/15 17:51:04 mclareni Exp $
*
* $Log: msbit1.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:04  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION MSBIT1 (IZW,IZP)
C
C CERN PROGLIB# M421    MSBIT1          .VERSION KERNAPO  1.10  870524
C ORIG. 19/05/87  JZ
C
      MSBIT1 = OR (IZW, LSHFT(1,IZP-1))
      RETURN
      END
#ifdef CERNLIB_TCGEN_MSBIT1
#undef CERNLIB_TCGEN_MSBIT1
#endif
