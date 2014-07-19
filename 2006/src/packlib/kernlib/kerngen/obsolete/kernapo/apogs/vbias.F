*
* $Id: vbias.F,v 1.1.1.1 1996/02/15 17:51:07 mclareni Exp $
*
* $Log: vbias.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:07  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE VBIAS (B,ALPHA,A,N)
C
C CERN PROGLIB# F121    VBIAS           .VERSION KERNAPO  1.04  850321
C ORIG. 01/03/85  R.BRUN
C
      DIMENSION A(*),B(*)
      IF (N.GT.0) CALL VEC_$ADD_CONSTANT (B,N,ALPHA,A)
      END
#ifdef CERNLIB_TCGEN_VBIAS
#undef CERNLIB_TCGEN_VBIAS
#endif
