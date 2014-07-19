*
* $Id: mcbyt.F,v 1.1.1.1 1996/02/15 17:51:04 mclareni Exp $
*
* $Log: mcbyt.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:04  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION MCBYT (MZ,IZQ,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    MCBYT           .VERSION KERNAPO  1.10  870524
C ORIG. 19/05/87  JZ
C

      MCBYT = OR ( AND (IZW
     +                , NOT(LSHFT (RSHFT(NOT(INT4(0)),32-NZB), IZP-1)))
     +          ,  RSHFT (LSHFT(RSHFT(MZ,IZQ-1),32-NZB), 33-IZP-NZB))
      RETURN
      END
#ifdef CERNLIB_TCGEN_MCBYT
#undef CERNLIB_TCGEN_MCBYT
#endif
