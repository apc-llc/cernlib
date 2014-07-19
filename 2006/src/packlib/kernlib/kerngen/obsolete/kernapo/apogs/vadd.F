*
* $Id: vadd.F,v 1.1.1.1 1996/02/15 17:51:07 mclareni Exp $
*
* $Log: vadd.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:07  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE VADD (B,C,A,N)
C
C CERN PROGLIB# F121    VADD            .VERSION KERNAPO  1.04  850321
C ORIG. 01/03/85  R.BRUN
C
      DIMENSION A(*),B(*),C(*)
      IF (N.GT.0) CALL VEC_$ADD_VECTOR (B,C,N,A)
      END
#ifdef CERNLIB_TCGEN_VADD
#undef CERNLIB_TCGEN_VADD
#endif
