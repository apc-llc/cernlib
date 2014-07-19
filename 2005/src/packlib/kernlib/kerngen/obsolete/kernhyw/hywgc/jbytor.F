*
* $Id: jbytor.F,v 1.1.1.1 1996/02/15 17:52:50 mclareni Exp $
*
* $Log: jbytor.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:50  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION JBYTOR (MZ,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    JBYTOR          .VERSION KERNHYW  1.05  870602
C ORIG. 19/05/87  JZ
C
      JBYTOR = IOR  (MZ, IBITS(IZW,IZP-1,NZB))
      RETURN
      END
#ifdef CERNLIB_TCGEN_JBYTOR
#undef CERNLIB_TCGEN_JBYTOR
#endif
*      ==================================================
#include "qcardl.inc"
