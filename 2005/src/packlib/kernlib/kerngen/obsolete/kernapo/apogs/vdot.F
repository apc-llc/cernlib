*
* $Id: vdot.F,v 1.1.1.1 1996/02/15 17:51:07 mclareni Exp $
*
* $Log: vdot.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:07  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION VDOT (A,B,N)
C
C CERN PROGLIB# F121    VDOT            .VERSION KERNAPO  1.17  890626
C ORIG. 30/05/89     FCA
C
      DIMENSION A(*), B(*)

      VDOT=0.0
      IF(N.GT.0) VDOT=VEC_$DOT(A,B,N)
      END
#ifdef CERNLIB_TCGEN_VDOT
#undef CERNLIB_TCGEN_VDOT
#endif
