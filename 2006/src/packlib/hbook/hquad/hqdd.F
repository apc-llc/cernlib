*
* $Id: hqdd.F,v 1.1.1.1 1996/01/16 17:08:00 mclareni Exp $
*
* $Log: hqdd.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:00  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 22/11/92  12.11.50  by  John Allison
*-- Author :
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION FUNCTION HQDD (V)
      DOUBLE PRECISION V (*)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL FUNCTION HQDD (V)
      REAL V (*)
#endif
* Determines sum of multiquadrics.
* V is DOUBLE PRECISION coordinate vector in plot coordinates.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcqcor.inc"
 
      INTEGER J
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION HQDDJ
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL HQDDJ
#endif
 
      HQDD = 0.
 
      DO 10    J = 1, NSIG
         HQDD = HQDD + SIGA (J) * HQDDJ (V, J)
   10 CONTINUE
 
      END
