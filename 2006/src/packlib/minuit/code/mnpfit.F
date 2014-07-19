*
* $Id: mnpfit.F,v 1.1.1.1 1996/03/07 14:31:31 mclareni Exp $
*
* $Log: mnpfit.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:31  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNPFIT(PARX2P,PARY2P,NPAR2P,COEF2P,SDEV2P)
#include "minuit/d506dp.inc"
C
C     to fit a parabola to npar2p points
C
C   npar2p   no. of points
C   parx2p(i)   x value of point i
C   pary2p(i)   y value of point i
C
C   coef2p(1...3)  coefficients of the fitted parabola
C   y=coef2p(1) + coef2p(2)*x + coef2p(3)*x**2
C   sdev2p= variance
C   method : chi**2 = min equation solved explicitly
      DIMENSION PARX2P(NPAR2P),PARY2P(NPAR2P),COEF2P(NPAR2P)
      DIMENSION CZ(3)
C
      DO 3  I=1,3
    3 CZ(I)=0.
      SDEV2P=0.
      IF(NPAR2P.LT.3) GO TO 10
      F=NPAR2P
C--- center x values for reasons of machine precision
      XM=0.
      DO 2  I=1,NPAR2P
    2 XM=XM+PARX2P(I)
      XM=XM/F
      X2=0.
      X3=0.
      X4=0.
      Y=0.
      Y2=0.
      XY=0.
      X2Y=0.
      DO 1  I=1,NPAR2P
      S=PARX2P(I)-XM
      T=PARY2P(I)
      S2=S*S
      X2=X2+S2
      X3=X3+S*S2
      X4=X4+S2*S2
      Y=Y+T
      Y2=Y2+T*T
      XY=XY+S*T
      X2Y=X2Y+S2*T
    1 CONTINUE
      A=(F*X4-X2**2)*X2-F*X3**2
      IF(A.EQ.0.)  GOTO 10
      CZ(3)=(X2*(F*X2Y-X2*Y)-F*X3*XY)/A
      CZ(2)=(XY-X3*CZ(3))/X2
      CZ(1)=(Y-X2*CZ(3))/F
      IF(NPAR2P.EQ.3)  GOTO 6
      SDEV2P=Y2-(CZ(1)*Y+CZ(2)*XY+CZ(3)*X2Y)
      IF(SDEV2P.LT.0.)  SDEV2P=0.
      SDEV2P=SDEV2P/(F-3.)
    6 CZ(1)=CZ(1)+XM*(XM*CZ(3)-CZ(2))
      CZ(2)=CZ(2)-2.*XM*CZ(3)
   10 CONTINUE
      DO 11  I=1,3
   11 COEF2P(I)=CZ(I)
      RETURN
      END
