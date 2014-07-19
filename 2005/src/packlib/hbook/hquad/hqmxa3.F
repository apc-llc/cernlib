*
* $Id: hqmxa3.F,v 1.1.1.1 1996/01/16 17:08:04 mclareni Exp $
*
* $Log: hqmxa3.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:04  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 22/11/92  12.11.50  by  John Allison
*-- Author :
      SUBROUTINE HQMXA3 (M, U, F, X)
      INTEGER M
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION U (M), F (M), X (*)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL U (M), F (M), X (*)
#endif
* Used by MaXimum likelihood routines for Gaussian integration in 3-D
*   with routines D110 (CERN library).
* A) Integrand is sum of multiquadrics.
 
#include "hbook/hcqcom.inc"
 
      INTEGER I
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION DGMLT2
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL RGMLT2
#endif
      EXTERNAL HQMXA2
 
      DO 10    I = 1, M
         X (3) = U (I)
#if defined(CERNLIB_DOUBLE)
         F (I) = DGMLT2 (HQMXA2, DBLE (YMI), DBLE (YMA),
     +   NINTVS, NQUADS, X)
#endif
#if !defined(CERNLIB_DOUBLE)
         F (I) = RGMLT2 (HQMXA2, YMI, YMA, NINTVS, NQUADS, X)
#endif
   10 CONTINUE
 
      END
