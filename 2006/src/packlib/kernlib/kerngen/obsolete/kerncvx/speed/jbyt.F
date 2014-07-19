*
* $Id: jbyt.F,v 1.1.1.1 1996/02/15 17:52:30 mclareni Exp $
*
* $Log: jbyt.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:30  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION JBYT (IW,J,NBITS)
C
C CERN PROGLIB# M421    JBYT            .VERSION KERNCVX  1.00  890315
C ORIG. 01/01/89  AGA+MAV, Convex
C
      JBYT = IBITS(IW,J-1,NBITS)
      RETURN
      END
#ifdef CERNLIB_TCGEN_JBYT
#undef CERNLIB_TCGEN_JBYT
#endif
