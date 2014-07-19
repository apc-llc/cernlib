*
* $Id: dqhid.F,v 1.1.1.1 1996/01/11 14:14:35 mclareni Exp $
*
* $Log: dqhid.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:35  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION DQHID(A2,S,TQ,UQ)
C     **************************
C-- QHI-QHI --+ QHI-QHI  ID.
C-- A2 = MASS**2  OF HEAVY QUARKS
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      DQHID=0.
      T=TQ+A2
      U=UQ+A2
      TUMN=-(.5*SQRT(S)-SQRT(.25*S-A2))**2
      IF(T.GT.TUMN) RETURN
      IF(U.GT.TUMN) RETURN
      S2=S**2
      T2=T**2
      U2=U**2
      DQHID=(4./9.)*((S2+U2-2.*(S+U)+A2**2)/T2
     1              +(S2+T2-2.*(S+T)+A2**2)/U2)
     2     -(8./27.)*(S2-3.*S*A2)/(U*T)
      RETURN
      END
