*
* $Id: mbytor.F,v 1.1.1.1 1996/02/15 17:54:49 mclareni Exp $
*
* $Log: mbytor.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:49  mclareni
* Kernlib
*
*
#include "kernnor/pilot.h"
      FUNCTION MBYTOR (MZ,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    MBYTOR          .VERSION KERNNOR  2.04  870524
C ORIG. 19/05/87  JZ
C
 
      MBYTOR = IOR (IZW, ISHFT (ISHFT(MZ,32-NZB), -33+IZP+NZB) )
      RETURN
      END
#ifdef CERNLIB_TCGEN_MBYTOR
#undef CERNLIB_TCGEN_MBYTOR
#endif
