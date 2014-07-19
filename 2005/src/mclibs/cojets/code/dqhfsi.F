*
* $Id: dqhfsi.F,v 1.1.1.1 1996/01/11 14:14:34 mclareni Exp $
*
* $Log: dqhfsi.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:34  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION DQHFSI(A2,S,TQ,UQ)
C     ***************************
C-- QHB-QH --+ QHB-QH  F=I
C-- A2 = MASS*2  OF HEAVY QUARKS
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      DQHFSI=0.
      T=TQ+A2
      U=UQ+A2
      TUMN=-(.5*SQRT(S)-SQRT(.25*S-A2))**2
      IF(T.GT.TUMN) RETURN
      IF(U.GT.TUMN) RETURN
      S2=S**2
      T2=T**2
      U2=U**2
      DQHFSI=(4./9.)*((U2+T2+2.*A2*(S-T-U+A2))/S2
     1               +(U2+S2-2.*A2*(U+S)+A2**2)/T2)
     2      -(8./27.)*(U2-A2*(2.*U-S)+A2**2)/(T*S)
      RETURN
      END
