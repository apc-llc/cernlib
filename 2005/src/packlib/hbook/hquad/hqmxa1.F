*
* $Id: hqmxa1.F,v 1.1.1.1 1996/01/16 17:08:03 mclareni Exp $
*
* $Log: hqmxa1.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:03  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 22/11/92  12.11.50  by  John Allison
*-- Author :
      SUBROUTINE HQMXA1 (M, U, F, X)
      INTEGER M
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION U (M), F (M), X (*)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL U (M), F (M), X (*)
#endif
* Used by MaXimum likelihood routines for Gaussian integration in 1-D
*   with routines D110 (CERN library).
* A) Integrand is sum of multiquadrics.
 
#include "hbook/hcqcom.inc"
 
      INTEGER I
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION HQDD
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL HQDD
#endif
 
      DO 10    I = 1, M
         X (1) = U (I)
         F (I) = HQDD (X)
   10 CONTINUE
 
      END
