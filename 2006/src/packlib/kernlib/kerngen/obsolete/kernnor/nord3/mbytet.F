*
* $Id: mbytet.F,v 1.1.1.1 1996/02/15 17:54:48 mclareni Exp $
*
* $Log: mbytet.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:48  mclareni
* Kernlib
*
*
#include "kernnor/pilot.h"
      FUNCTION MBYTET (MZ,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    MBYTET          .VERSION KERNNOR  2.04  870524
C ORIG. 19/05/87  JZ
C
 
      MBYTET = IAND (IZW,
     +               NOT(ISHFT (ISHFT(NOT(MZ),32-NZB), -33+IZP+NZB)))
      RETURN
      END
#ifdef CERNLIB_TCGEN_MBYTET
#undef CERNLIB_TCGEN_MBYTET
#endif
