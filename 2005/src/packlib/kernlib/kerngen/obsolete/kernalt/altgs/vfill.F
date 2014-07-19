*
* $Id: vfill.F,v 1.1.1.1 1996/02/15 17:50:55 mclareni Exp $
*
* $Log: vfill.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:55  mclareni
* Kernlib
*
*
#include "kernalt/pilot.h"
      SUBROUTINE VFILL (A,N,IVAL)
C
C CERN PROGLIB# F121    VFILL           .VERSION KERNALT  1.00  880212
C ORIG. 01/07/71
C
      INTEGER      A(99)
 
      IF (N.LE.0)            RETURN
 
      A(1:N) = IVAL
      RETURN
      END
#ifdef CERNLIB_TCGEN_VFILL
#undef CERNLIB_TCGEN_VFILL
#endif
