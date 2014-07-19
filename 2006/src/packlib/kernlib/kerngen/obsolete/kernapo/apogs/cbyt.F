*
* $Id: cbyt.F,v 1.1.1.1 1996/02/15 17:51:01 mclareni Exp $
*
* $Log: cbyt.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:01  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE CBYT (MZ,IZQ,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    CBYT            .VERSION KERNAPO  1.15  881020
C ORIG. 14/12/83  JZ
C
C
      MASK = LSHFT (RSHFT(INT4(-1),32-NZB), IZP-1)
      IZW  = OR ( AND(NOT(MASK),IZW),
     +            AND(MASK,LSHFT(RSHFT(MZ,IZQ-1),IZP-1)))
      RETURN
      END
#ifdef CERNLIB_TCGEN_CBYT
#undef CERNLIB_TCGEN_CBYT
#endif
