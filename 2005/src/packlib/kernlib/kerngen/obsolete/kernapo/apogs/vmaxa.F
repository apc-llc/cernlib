*
* $Id: vmaxa.F,v 1.1.1.1 1996/02/15 17:51:08 mclareni Exp $
*
* $Log: vmaxa.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:08  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION VMAXA (A,N)
C
C CERN PROGLIB# F121    VMAXA           .VERSION KERNAPO  1.17  890626
C ORIG. 30/05/89     FCA
C
      DIMENSION A(*)

      VMAXA = 0.0
      IF (N.GT.0) CALL VEC_$MAX (A,N,VMAXA,IMAXA)
      END
#ifdef CERNLIB_TCGEN_VMAXA
#undef CERNLIB_TCGEN_VMAXA
#endif
