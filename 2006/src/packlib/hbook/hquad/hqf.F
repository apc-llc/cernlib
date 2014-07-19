*
* $Id: hqf.F,v 1.1.1.1 1996/01/16 17:08:00 mclareni Exp $
*
* $Log: hqf.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:00  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/02 18/12/92  15.47.59  by  John Allison
*-- Author :
      REAL FUNCTION HQF (V)
      REAL V (*)
* Determines sum of multiquadrics.
* V is coordinate vector in plot coordinates.
 
#include "hbook/hcqcom.inc"
 
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION HQD
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL HQD
#endif
 
      HQF = 0.
      IF (IMQFUN .EQ. 1) THEN
         HQF = HQD (V)
      ELSE IF (IMQFUN .EQ. 2) THEN
         HQF = VCONST * EXP (HQD (V))
      END IF
 
      END
