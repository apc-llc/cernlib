*
* $Id: uswop.F,v 1.1.1.1 1996/02/15 17:51:06 mclareni Exp $
*
* $Log: uswop.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:06  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE USWOP (IA,IB,N)
C
C CERN PROGLIB# F121    USWOP           .VERSION KERNAPO  1.16  890321
C ORIG. 19/12/88  F.Carminati
C
      DIMENSION IA(*), IB(*)
      IF (N.GT.0)  CALL VEC_$ISWAP(IA,IB,N)
      END
#ifdef CERNLIB_TCGEN_USWOP
#undef CERNLIB_TCGEN_USWOP
#endif
