*
* $Id: vcopyn.F,v 1.1.1.1 1996/02/15 17:51:07 mclareni Exp $
*
* $Log: vcopyn.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:07  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE VCOPYN (A,X,N)
C
C CERN PROGLIB# F121    VCOPYN          .VERSION KERNAPO  1.17  890626
C ORIG. 30/05/89     FCA
C
      DIMENSION A(*), B(*)

      IF (N.GT.0) CALL VEC_$MULT_CONSTANT (A,N,-1.,X)
      END
#ifdef CERNLIB_TCGEN_VCOPYN
#undef CERNLIB_TCGEN_VCOPYN
#endif
