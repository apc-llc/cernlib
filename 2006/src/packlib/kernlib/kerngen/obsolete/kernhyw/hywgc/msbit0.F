*
* $Id: msbit0.F,v 1.1.1.1 1996/02/15 17:52:52 mclareni Exp $
*
* $Log: msbit0.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:52  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION MSBIT0 (IZW,IZP)
C
C CERN PROGLIB# M421    MSBIT0          .VERSION KERNHYW  1.05  870602
C ORIG. 19/05/87  JZ
C
      MSBIT0 = IAND (IZW, NOT(ILS(1,IZP-1)) )
      RETURN
      END
#ifdef CERNLIB_TCGEN_MSBIT0
#undef CERNLIB_TCGEN_MSBIT0
#endif
*      ==================================================
#include "qcardl.inc"
