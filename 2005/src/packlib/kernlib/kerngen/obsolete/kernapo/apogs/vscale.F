*
* $Id: vscale.F,v 1.1.1.1 1996/02/15 17:51:08 mclareni Exp $
*
* $Log: vscale.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:08  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE VSCALE (A,ALPHA,X,N)
C
C CERN PROGLIB# F121    VSCALE          .VERSION KERNAPO  1.17  890626
C ORIG. 30/05/89     FCA
C
      DIMENSION A(*), X(*)

      IF (N.GT.0) CALL VEC_$MULT_CONSTANT (A,N,ALPHA,X)
      END
#ifdef CERNLIB_TCGEN_VSCALE
#undef CERNLIB_TCGEN_VSCALE
#endif
