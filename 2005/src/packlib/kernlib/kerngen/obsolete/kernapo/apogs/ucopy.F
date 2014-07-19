*
* $Id: ucopy.F,v 1.1.1.1 1996/02/15 17:51:06 mclareni Exp $
*
* $Log: ucopy.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:06  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE UCOPY (A,B,N)
C
C CERN PROGLIB# V301    UCOPY           .VERSION KERNAPO  1.24  920511
C ORIG. 01/03/85  R.BRUN
C
      INTEGER      A(*), B(*)

      IF (N.GT.0) CALL VEC_$COPY (A,B,N)
      END
#ifdef CERNLIB_TCGEN_UCOPY
#undef CERNLIB_TCGEN_UCOPY
#endif
