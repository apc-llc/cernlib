*
* $Id: dsexg.F,v 1.1.1.1 1996/01/11 14:14:35 mclareni Exp $
*
* $Log: dsexg.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:35  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION DSEXG(A2,S,T,UQ)
C     *************************
C-- G-QH --+ G-QH ,  A2=MASS*2 OF HEAVY QUARK
C-- ZERO INITIAL MASSES
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      DSEXG=0.
      U=UQ+2.*A2
      UMN=0.
      IF(U.GT.UMN) RETURN
      DSEXG=-2.*S*U/T**2
     1 +(4./9.)*(-S*U+A2*(U-2.*S))/(A2-S)**2
     2 +(4./9.)*(-S*U+A2*(S-2.*U))/(A2-U)**2
     3 -(1./9.)*A2*(S+U)/((A2-U)*(A2-S))
     4 +(S*U+1.5*S*A2)/(T*(A2-S))
     5 +(S*U+1.5*U*A2)/(T*(A2-U))
      RETURN
      END
