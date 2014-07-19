*
* $Id: vmatr.F,v 1.1.1.1 1996/02/15 17:51:08 mclareni Exp $
*
* $Log: vmatr.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:08  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE VMATR (A,G,V,N,M)
C
C CERN PROGLIB# F121    VMATR           .VERSION KERNAPO  1.17  890626
C ORIG. 30/05/89     FCA
C
      DIMENSION A(*), G(M,N), V(*)

      IF (N.GT.0.AND.M.GT.0) CALL VEC_$POSTMULTN (G,A,M,N,V)
      END
#ifdef CERNLIB_TCGEN_VMATR
#undef CERNLIB_TCGEN_VMATR
#endif
