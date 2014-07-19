*
* $Id: sbytor.F,v 1.1.1.1 1996/02/15 17:51:05 mclareni Exp $
*
* $Log: sbytor.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:05  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE SBYTOR (IT,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    SBYTOR          .VERSION KERNAPO  1.00  831212
C ORIG. 12/12/83  JZ
C

      IZW = OR (IZW, LSHFT(AND(RSHFT(INT4(-1),32-NZB),IT), IZP-1))
      RETURN
      END
#ifdef CERNLIB_TCGEN_SBYTOR
#undef CERNLIB_TCGEN_SBYTOR
#endif
