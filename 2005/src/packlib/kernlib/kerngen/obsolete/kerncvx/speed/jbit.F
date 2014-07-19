*
* $Id: jbit.F,v 1.1.1.1 1996/02/15 17:52:30 mclareni Exp $
*
* $Log: jbit.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:30  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION JBIT (IW,J)
C
C CERN PROGLIB# M421    JBIT            .VERSION KERNCVX  1.00  890315
C ORIG. 01/01/89  AGA+MAV, Convex
C
      JBYT = IBITS (IW,J-1,1)
      RETURN
      END
#ifdef CERNLIB_TCGEN_JBIT
#undef CERNLIB_TCGEN_JBIT
#endif
