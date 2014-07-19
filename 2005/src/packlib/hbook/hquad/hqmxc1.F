*
* $Id: hqmxc1.F,v 1.1.1.1 1996/01/16 17:08:04 mclareni Exp $
*
* $Log: hqmxc1.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:04  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 22/11/92  12.11.50  by  John Allison
*-- Author :
      SUBROUTINE HQMXC1 (M, U, F, X)
      INTEGER M
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION U (M), F (M), X (*)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL U (M), F (M), X (*)
#endif
* Used by MaXimum likelihood routines for Gaussian integration in 1-D
*   with routines D110 (CERN library).
* C) Integrand is e^s phi_j phi_k.
 
#include "hbook/hcqcom.inc"
 
      INTEGER I
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION HQDD, HQDDJ
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL HQDD, HQDDJ
#endif
 
      DO 10    I = 1, M
         X (1) = U (I)
         F (I) = EXP (HQDD (X)) * HQDDJ (X, JMQ) * HQDDJ (X, KMQ)
   10 CONTINUE
 
      END
