*
* $Id: ucopy.F,v 1.1.1.1 1996/02/15 17:50:55 mclareni Exp $
*
* $Log: ucopy.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:55  mclareni
* Kernlib
*
*
#include "kernalt/pilot.h"
      SUBROUTINE UCOPY (A,B,N)
C
C CERN PROGLIB# V301    UCOPY           .VERSION KERNALT  1.00  880212
C ORIG. 15/02/88 JZ
C
C--   No overlap, A and B must be separate vectors on the Alliant
C-    For overlaps use UCOPY2
 
      DIMENSION A(2),B(2)
 
      IF (N.EQ.0) RETURN
      B(1:N) = A(1:N)
      RETURN
      END
#ifdef CERNLIB_TCGEN_UCOPY
#undef CERNLIB_TCGEN_UCOPY
#endif
