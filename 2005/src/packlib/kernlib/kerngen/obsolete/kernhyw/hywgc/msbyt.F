*
* $Id: msbyt.F,v 1.1.1.1 1996/02/15 17:52:52 mclareni Exp $
*
* $Log: msbyt.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:52  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION MSBYT (MZ,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    MSBYT           .VERSION KERNHYW  1.05  870602
C ORIG. 19/05/87  JZ
C
 
      MSBYT = IOR ( IAND (IZW,
     +                    NOT(ILS (IRL(NOT(0),36-NZB), IZP-1)))
     +,             IRL (ILS(MZ, 36-NZB), 37-IZP-NZB) )
      RETURN
      END
#ifdef CERNLIB_TCGEN_MSBYT
#undef CERNLIB_TCGEN_MSBYT
#endif
*      ==================================================
#include "qcardl.inc"
