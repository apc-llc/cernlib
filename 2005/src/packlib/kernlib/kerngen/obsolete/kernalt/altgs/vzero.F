*
* $Id: vzero.F,v 1.1.1.1 1996/02/15 17:50:55 mclareni Exp $
*
* $Log: vzero.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:55  mclareni
* Kernlib
*
*
#include "kernalt/pilot.h"
      SUBROUTINE VZERO (IA,N)
C
C CERN PROGLIB# F121    VZERO           .VERSION KERNALT  1.00  880212
C ORIG. 01/07/71, modif. 24/05/87 to set integer zero
C
      DIMENSION    IA(9)
 
      IF (N.LE.0)            RETURN
 
      IA(1:N) = 0
      RETURN
      END
#ifdef CERNLIB_TCGEN_VZERO
#undef CERNLIB_TCGEN_VZERO
#endif
