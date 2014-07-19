*
* $Id: vzero.F,v 1.1.1.1 1996/02/15 17:51:08 mclareni Exp $
*
* $Log: vzero.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:08  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE VZERO(A,N)
C
C CERN PROGLIB# F121    VZERO           .VERSION KERNAPO  1.24  920511
C ORIG. 01/03/85  R.BRUN
C
      INTEGER      A(*)

      IF (N.GT.0) CALL VEC_$ZERO (A,N)
      END
#ifdef CERNLIB_TCGEN_VZERO
#undef CERNLIB_TCGEN_VZERO
#endif
