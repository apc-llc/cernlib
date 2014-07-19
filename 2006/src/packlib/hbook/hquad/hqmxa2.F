*
* $Id: hqmxa2.F,v 1.1.1.1 1996/01/16 17:08:04 mclareni Exp $
*
* $Log: hqmxa2.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:04  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 22/11/92  12.11.50  by  John Allison
*-- Author :
      SUBROUTINE HQMXA2 (M, U, F, X)
      INTEGER M
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION U (M), F (M), X (*)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL U (M), F (M), X (*)
#endif
* Used by MaXimum likelihood routines for Gaussian integration in 2-D
*   with routines D110 (CERN library).
* A) Integrand is sum of multiquadrics.
 
#include "hbook/hcqcom.inc"
 
      INTEGER I
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION DGMLT1
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL RGMLT1
#endif
      EXTERNAL HQMXA1
 
      DO 10    I = 1, M
         X (2) = U (I)
#if defined(CERNLIB_DOUBLE)
         F (I) = DGMLT1 (HQMXA1, DBLE (XMI), DBLE (XMA),
     +   NINTVS, NQUADS, X)
#endif
#if !defined(CERNLIB_DOUBLE)
         F (I) = RGMLT1 (HQMXA1, XMI, XMA, NINTVS, NQUADS, X)
#endif
   10 CONTINUE
 
      END
