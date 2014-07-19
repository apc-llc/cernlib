*
* $Id: dsexq.F,v 1.1.1.1 1996/01/11 14:14:35 mclareni Exp $
*
* $Log: dsexq.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:35  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION DSEXQ(A2,S,T,UQ)
C     *************************
C-- Q-QH --+ Q-QH ,  A2=MASS*2 OF HEAVY QUARK
C-- ZERO INITIAL MASSES
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      DSEXQ=0.
      U=UQ+2.*A2
      UMN=0.
      IF(U.GT.UMN) RETURN
      DSEXQ=(4./9.)*(S**2+U**2+4.*T*A2)/T**2
      RETURN
      END
