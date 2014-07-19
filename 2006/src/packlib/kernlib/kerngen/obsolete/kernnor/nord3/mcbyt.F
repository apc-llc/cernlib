*
* $Id: mcbyt.F,v 1.1.1.1 1996/02/15 17:54:49 mclareni Exp $
*
* $Log: mcbyt.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:49  mclareni
* Kernlib
*
*
#include "kernnor/pilot.h"
      FUNCTION MCBYT (MZ,IZQ,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    MCBYT           .VERSION KERNNOR  2.04  870524
C ORIG. 19/05/87  JZ
C
 
      MCBYT = IOR ( IAND (IZW,
     +                    NOT(ISHFT (ISHFT(NOT(0),-32+NZB), IZP-1)))
     +,         ISHFT (ISHFT(ISHFT(MZ,-IZQ+1),32-NZB), -33+IZP+NZB) )
      RETURN
      END
#ifdef CERNLIB_TCGEN_MCBYT
#undef CERNLIB_TCGEN_MCBYT
#endif
