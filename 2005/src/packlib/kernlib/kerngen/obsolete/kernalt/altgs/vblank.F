*
* $Id: vblank.F,v 1.1.1.1 1996/02/15 17:50:55 mclareni Exp $
*
* $Log: vblank.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:55  mclareni
* Kernlib
*
*
#include "kernalt/pilot.h"
      SUBROUTINE VBLANK (A,N)
C
C CERN PROGLIB# F121    VBLANK          .VERSION KERNALT  1.00  880212
C ORIG. 01/07/71
C
      INTEGER      A(99), BLANK
      PARAMETER    (BLANK = '20202020'X)
 
      IF (N.LE.0)  RETURN
 
      A(1:N) = BLANK
      RETURN
      END
#ifdef CERNLIB_TCGEN_VBLANK
#undef CERNLIB_TCGEN_VBLANK
#endif
