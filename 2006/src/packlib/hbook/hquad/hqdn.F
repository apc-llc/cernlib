*
* $Id: hqdn.F,v 1.1.1.1 1996/01/16 17:08:00 mclareni Exp $
*
* $Log: hqdn.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:00  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 22/11/92  12.11.50  by  John Allison
*-- Author :
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION FUNCTION HQDN (V)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL FUNCTION HQDN (V)
#endif
      REAL V (*)
* Determines sum of multiquadrics.
* V is coordinate vector in NORMALISED coordinates.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcqcor.inc"
 
      INTEGER J
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION HQDJN
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL HQDJN
#endif
 
      HQDN = 0.
 
      DO 10    J = 1, NSIG
         HQDN = HQDN + SIGA (J) * HQDJN (V, J)
   10 CONTINUE
 
      END
