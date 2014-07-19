*
* $Id: jbit.F,v 1.1.1.1 1996/02/15 17:51:02 mclareni Exp $
*
* $Log: jbit.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:02  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION JBIT (IZW,IZP)
C
C CERN PROGLIB# M421    JBIT            .VERSION KERNAPO  1.00  831212
C ORIG. 12/12/83  JZ
C
      JBIT = AND (RSHFT(IZW,IZP-1), 1)
      RETURN
      END
#ifdef CERNLIB_TCGEN_JBIT
#undef CERNLIB_TCGEN_JBIT
#endif
