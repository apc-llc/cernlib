*
* $Id: ddang.F,v 1.1.1.1 1996/01/11 14:14:34 mclareni Exp $
*
* $Log: ddang.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:34  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE DDANG(THETA)
C     ***********************
C-- GENERATES THETA ANGLE IN DRELL-YAN PHOTON DECAYS
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      X=CJRN(0.)
      IF(CJRN(0.)*4./3..LT.1.) GO TO 10
      X=X**(1./3.)
10    X=X*(-1.)**INT(1.+2.*CJRN(1.))
      THETA=ACOSX(X)
      RETURN
      END
