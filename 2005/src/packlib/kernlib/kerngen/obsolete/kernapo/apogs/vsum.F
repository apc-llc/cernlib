*
* $Id: vsum.F,v 1.1.1.1 1996/02/15 17:51:08 mclareni Exp $
*
* $Log: vsum.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:08  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION VSUM (A,N)
C
C CERN PROGLIB# F121    VSUM            .VERSION KERNAPO  1.04  850321
C ORIG. 01/03/85  R.BRUN
C
      DIMENSION A(*)
      IF (N.GT.0) THEN
        VSUM= VEC_$SUM(A,N)
      ELSE
        VSUM= 0.0
      ENDIF
      END
#ifdef CERNLIB_TCGEN_VSUM
#undef CERNLIB_TCGEN_VSUM
#endif
