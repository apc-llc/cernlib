*
* $Id: mbytor.F,v 1.1.1.1 1996/02/15 17:52:52 mclareni Exp $
*
* $Log: mbytor.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:52  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION MBYTOR (MZ,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    MBYTOR          .VERSION KERNHYW  1.05  870602
C ORIG. 19/05/87  JZ
C
 
      MBYTOR = IOR (IZW, IRL (ILS(MZ,36-NZB), 37-IZP-NZB) )
      RETURN
      END
#ifdef CERNLIB_TCGEN_MBYTOR
#undef CERNLIB_TCGEN_MBYTOR
#endif
*      ==================================================
#include "qcardl.inc"
