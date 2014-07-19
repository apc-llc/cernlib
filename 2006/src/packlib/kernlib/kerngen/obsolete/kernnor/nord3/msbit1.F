*
* $Id: msbit1.F,v 1.1.1.1 1996/02/15 17:54:49 mclareni Exp $
*
* $Log: msbit1.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:49  mclareni
* Kernlib
*
*
#include "kernnor/pilot.h"
      FUNCTION MSBIT1 (IZW,IZP)
C
C CERN PROGLIB# M421    MSBIT1          .VERSION KERNNOR  2.04  870524
C ORIG. 19/05/87  JZ
C
      MSBIT1 = IOR  (IZW, ISHFT(1,IZP-1) )
      RETURN
      END
#ifdef CERNLIB_TCGEN_MSBIT1
#undef CERNLIB_TCGEN_MSBIT1
#endif
