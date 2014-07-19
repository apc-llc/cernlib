*
* $Id: vfill.F,v 1.1.1.1 1996/02/15 17:51:07 mclareni Exp $
*
* $Log: vfill.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:07  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE VFILL (A,N,C)
C
C CERN PROGLIB# F121    VFILL           .VERSION KERNAPO  1.24  920511
C ORIG. 01/03/85  R.BRUN
C
      INTEGER      A(*)

      IF (N.GT.0) CALL VEC_$INIT (A,N,C)
      END
#ifdef CERNLIB_TCGEN_VFILL
#undef CERNLIB_TCGEN_VFILL
#endif
