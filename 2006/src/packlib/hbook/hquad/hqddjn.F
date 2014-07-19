*
* $Id: hqddjn.F,v 1.1.1.1 1996/01/16 17:08:00 mclareni Exp $
*
* $Log: hqddjn.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:00  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 22/11/92  12.11.50  by  John Allison
*-- Author :
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION FUNCTION HQDDJN (V, J)
      DOUBLE PRECISION V (*)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL FUNCTION HQDDJN (V, J)
      REAL V (*)
#endif
      INTEGER J
* Determines J'th multiquadric.
* V is DOUBLE PRECISION coordinate vector in NORMALISED coordinates.
 
#include "hbook/hcqcom.inc"
 
      INTEGER I
 
      HQDDJN = 0.
 
      DO 10    I = 1, NDIM
         HQDDJN = HQDDJN +
     +   (V (I) - DBLE (SIGV (J, I))) ** 2
   10 CONTINUE
 
      HQDDJN = HQDDJN + DBLE (SIGDEL (J)) ** 2
 
      HQDDJN = SQRT (HQDDJN)
 
      END
