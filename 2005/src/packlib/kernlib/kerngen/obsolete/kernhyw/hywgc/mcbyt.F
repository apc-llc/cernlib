*
* $Id: mcbyt.F,v 1.1.1.1 1996/02/15 17:52:52 mclareni Exp $
*
* $Log: mcbyt.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:52  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION MCBYT (MZ,IZQ,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    MCBYT           .VERSION KERNHYW  1.05  870602
C ORIG. 19/05/87  JZ
C
 
      MCBYT = IOR ( IAND (IZW,
     +                    NOT(ILS (IRL(NOT(0),36-NZB), IZP-1)))
     +,             IRL (ILS(IRL(MZ,IZQ-1),36-NZB), 37-IZP-NZB) )
      RETURN
      END
#ifdef CERNLIB_TCGEN_MCBYT
#undef CERNLIB_TCGEN_MCBYT
#endif
*      ==================================================
#include "qcardl.inc"
