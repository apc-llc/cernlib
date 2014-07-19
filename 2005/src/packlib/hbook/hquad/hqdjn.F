*
* $Id: hqdjn.F,v 1.1.1.1 1996/01/16 17:08:00 mclareni Exp $
*
* $Log: hqdjn.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:00  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 22/11/92  12.11.50  by  John Allison
*-- Author :
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION FUNCTION HQDJN (V, J)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL FUNCTION HQDJN (V, J)
#endif
      REAL V (*)
      INTEGER J
* Determines J'th multiquadric.
* V is coordinate vector in NORMALISED coordinates.
 
#include "hbook/hcqcom.inc"
 
      INTEGER I
 
      HQDJN = 0.
 
      DO 10    I = 1, NDIM
         HQDJN = HQDJN +
     +   (DBLE (V (I)) - DBLE (SIGV (J, I))) ** 2
   10 CONTINUE
 
      HQDJN = HQDJN + DBLE (SIGDEL (J)) ** 2
 
      HQDJN = SQRT (HQDJN)
 
      END
