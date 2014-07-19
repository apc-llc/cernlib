*
* $Id: vmatl.F,v 1.1.1.1 1996/02/15 17:51:07 mclareni Exp $
*
* $Log: vmatl.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:07  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE VMATL (G,C,X,N,M)
C
C CERN PROGLIB# F121    VMATL           .VERSION KERNAPO  1.17  890626
C ORIG. 30/05/89     FCA
C
      DIMENSION G(M,N), C(*), X(*)

      IF (N.GT.0.AND.M.GT.0) CALL VEC_$PREMULTN (C,G,M,N,X)
      END
#ifdef CERNLIB_TCGEN_VMATL
#undef CERNLIB_TCGEN_VMATL
#endif
