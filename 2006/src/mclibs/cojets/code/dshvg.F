*
* $Id: dshvg.F,v 1.1.1.1 1996/01/11 14:14:35 mclareni Exp $
*
* $Log: dshvg.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:35  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION DSHVG(A2,S,TQ,UQ)
C     **************************
C-- G-G --+ QHB-QH ,  A2=MASS*2 OF HEAVY QUARK
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      DSHVG=0.
      T=TQ+A2
      U=UQ+A2
      TUMN=-(.5*SQRT(S)-SQRT(.25*S-A2))**2
      IF(T.GT.TUMN) RETURN
      IF(U.GT.TUMN) RETURN
      DSHVG=(3./4.)*TQ*UQ/S**2
     1+(1./6.)*(TQ*UQ-2.*A2*(2.*A2+TQ))/TQ**2
     2+(1./6.)*(TQ*UQ-2.*A2*(2.*A2+UQ))/UQ**2
     3-(1./24.)*A2*(S-4.*A2)/(TQ*UQ)
     4+(3./8.)*(TQ*UQ+A2*(UQ-TQ))/(S*TQ)
     5+(3./8.)*(TQ*UQ+A2*(TQ-UQ))/(S*UQ)
      RETURN
      END
