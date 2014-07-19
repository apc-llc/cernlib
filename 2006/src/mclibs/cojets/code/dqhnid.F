*
* $Id: dqhnid.F,v 1.1.1.1 1996/01/11 14:14:35 mclareni Exp $
*
* $Log: dqhnid.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:35  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION DQHNID(A2,B2,S,TQ,UQ)
C     ******************************
C-- QHI-QHJ --+ QHI-QHJ  NON-ID.
C-- A2, B2 = MASS**2 OF FINAL HEAVY QUARKS
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      DQHNID=0.
      AB2=0.5*(A2+B2)
      T=TQ+AB2
      U=UQ+AB2
      TUMN=((A2-B2)**2-(S-SQRT((S+A2-B2)**2-4.*S*A2))**2)/(4.*S)
      IF(T.GT.TUMN) RETURN
      IF(U.GT.TUMN) RETURN
      DQHNID=(4./9.)*(S**2+U**2-(A2+B2)*(S+U)+A2*B2)/T**2
      RETURN
      END
