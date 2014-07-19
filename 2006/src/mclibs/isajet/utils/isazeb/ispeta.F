*
* $Id: ispeta.F,v 1.10 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: ispeta.F,v $
* Revision 1.10  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE ISPETA(P,THETA,PHI,ETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     convert a momentum P(3) to theta, phi and eta (pseudo-rapidity)
C-   Inputs  :
C-     P(3) = px, py, pz
C-   Outputs :
C-    THETA = polar angle
C-    PHI   = atan(px,py) range 0 to twopi
C-    ETA   = pseudo-rapidity
C-
C-   Created  13-DEC-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      REAL TWOPI
      PARAMETER (TWOPI=6.283185)
      REAL    P(3),THETA,PHI,ETA
      REAL    PTOT,PZOP
C----------------------------------------------------------------------
C
      PHI=ATAN2(P(2),P(1)+.00001) ! protection
      IF(PHI.LT.0) PHI=PHI+TWOPI
      PTOT=SQRT(P(1)**2+P(2)**2+P(3)**2)
      PZOP=P(3)/(PTOT+.0001)    ! protect against 0 momentum
      THETA=ACOS(PZOP)
C             protect against small theta
      ETA=10.*SIGN(1.,PZOP)
      IF(ABS(PZOP).LT.0.9999) ETA=-ALOG(TAN(THETA/2.))
C
  999 RETURN
      END
