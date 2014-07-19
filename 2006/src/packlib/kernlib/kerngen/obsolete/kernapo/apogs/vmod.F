*
* $Id: vmod.F,v 1.1.1.1 1996/02/15 17:51:08 mclareni Exp $
*
* $Log: vmod.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:08  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION VMOD (A,N)
C
C CERN PROGLIB# F121    VMOD            .VERSION KERNAPO  1.17  890626
C ORIG. 30/05/89     FCA
C
      DIMENSION A(*)

      VMOD=0.0
      IF(N.GT.0) VMOD=SQRT(VEC_$DOT(A,A,N))
      END
#ifdef CERNLIB_TCGEN_VMOD
#undef CERNLIB_TCGEN_VMOD
#endif
