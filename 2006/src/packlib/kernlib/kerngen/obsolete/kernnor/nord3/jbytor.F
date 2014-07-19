*
* $Id: jbytor.F,v 1.1.1.1 1996/02/15 17:54:48 mclareni Exp $
*
* $Log: jbytor.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:48  mclareni
* Kernlib
*
*
#include "kernnor/pilot.h"
      FUNCTION JBYTOR (MZ,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    JBYTOR          .VERSION KERNNOR  2.04  870524
C ORIG. 19/05/87  JZ
C
      JBYTOR  = IOR  (MZ, ISHFT (ISHFT(IZW,33-IZP-NZB),-32+NZB) )
      RETURN
      END
#ifdef CERNLIB_TCGEN_JBYTOR
#undef CERNLIB_TCGEN_JBYTOR
#endif
