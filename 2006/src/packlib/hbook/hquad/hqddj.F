*
* $Id: hqddj.F,v 1.1.1.1 1996/01/16 17:08:00 mclareni Exp $
*
* $Log: hqddj.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:00  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 22/11/92  12.11.50  by  John Allison
*-- Author :
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION FUNCTION HQDDJ (V, J)
      DOUBLE PRECISION V (*)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL FUNCTION HQDDJ (V, J)
      REAL V (*)
#endif
      INTEGER J
* Determines J'th multiquadric.
* V is DOUBLE PRECISION coordinate vector in plot coordinates.
 
#include "hbook/hcqcom.inc"
 
      INTEGER IDIM
* V1 is coordinate vector in NORMALISED coordinates.
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION V1 (NDMAX), HQDDJN
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL V1 (NDMAX), HQDDJN
#endif
 
      DO 10    IDIM = 1, NDIM
         V1 (IDIM) = (V (IDIM) - DBLE (SIGVMI (IDIM))) /
     +   DBLE (SIGVT (IDIM))
   10 CONTINUE
 
      HQDDJ = HQDDJN (V1, J)
 
      END
