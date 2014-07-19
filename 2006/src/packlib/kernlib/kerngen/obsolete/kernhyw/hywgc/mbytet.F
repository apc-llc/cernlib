*
* $Id: mbytet.F,v 1.1.1.1 1996/02/15 17:52:52 mclareni Exp $
*
* $Log: mbytet.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:52  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION MBYTET (MZ,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    MBYTET          .VERSION KERNHYW  1.05  870602
C ORIG. 19/05/87  JZ
C
 
      MBYTET = IAND (IZW,
     +               NOT(IRL (ILS(NOT(MZ),36-NZB), 37-IZP-NZB)))
      RETURN
      END
#ifdef CERNLIB_TCGEN_MBYTET
#undef CERNLIB_TCGEN_MBYTET
#endif
*      ==================================================
#include "qcardl.inc"
