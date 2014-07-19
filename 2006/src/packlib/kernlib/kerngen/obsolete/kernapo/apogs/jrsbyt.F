*
* $Id: jrsbyt.F,v 1.1.1.1 1996/02/15 17:51:03 mclareni Exp $
*
* $Log: jrsbyt.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:03  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION JRSBYT (IT,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    JRSBYT          .VERSION KERNAPO  1.01  831216
C ORIG. 14/12/83  JZ
C

      MASK   = LSHFT (RSHFT(INT4(-1),32-NZB), IZP-1)
      JRSBYT = RSHFT (AND(MASK,IZW),IZP-1)
      IZW    = OR (AND(NOT(MASK),IZW), AND(MASK,LSHFT(IT,IZP-1)))
      RETURN
      END
#ifdef CERNLIB_TCGEN_JRSBYT
#undef CERNLIB_TCGEN_JRSBYT
#endif
