*
* $Id: hqdj.F,v 1.1.1.1 1996/01/16 17:08:00 mclareni Exp $
*
* $Log: hqdj.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:00  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 22/11/92  12.11.50  by  John Allison
*-- Author :
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION FUNCTION HQDJ (V, J)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL FUNCTION HQDJ (V, J)
#endif
      REAL V (*)
      INTEGER J
* Determines J'th multiquadric.
* V is coordinate vector in plot coordinates.
 
#include "hbook/hcqcom.inc"
 
      INTEGER IDIM
* V1 is coordinate vector in NORMALISED coordinates.
      REAL V1 (NDMAX)
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION HQDJN
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL HQDJN
#endif
 
      DO 10    IDIM = 1, NDIM
         V1 (IDIM) = (V (IDIM) - SIGVMI (IDIM)) / SIGVT (IDIM)
   10 CONTINUE
 
      HQDJ = HQDJN (V1, J)
 
      END
