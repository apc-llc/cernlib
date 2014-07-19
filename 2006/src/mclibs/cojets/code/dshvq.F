*
* $Id: dshvq.F,v 1.1.1.1 1996/01/11 14:14:35 mclareni Exp $
*
* $Log: dshvq.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:35  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION DSHVQ(A2,S,TQ,UQ)
C     **************************
C-- QB-Q --+ QHB-QH ,  A2=MASS**2 OF HEAVY QUARK
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      DSHVQ=0.
      T=TQ+A2
      U=UQ+A2
      TUMN=-(.5*SQRT(S)-SQRT(.25*S-A2))**2
      IF(T.GT.TUMN) RETURN
      IF(U.GT.TUMN) RETURN
      DSHVQ=(4./9.)*(TQ**2+UQ**2+2.*A2*S)/S**2
      RETURN
      END
