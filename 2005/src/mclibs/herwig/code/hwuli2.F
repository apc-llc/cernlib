CDECK  ID>, HWULI2.
*CMZ :-        -23/08/94  13.22.29  by  Mike Seymour
*-- Author :    Ulrich Baur & Nigel Glover, adapted by Ian Knowles
C-----------------------------------------------------------------------
      FUNCTION HWULI2(X)
C-----------------------------------------------------------------------
C     Complex dilogarithm function, Li_2 (Spence function)
C-----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE COMPLEX HWULI2,PROD,Y,Y2,X,Z
      DOUBLE PRECISION XR,XI,R2,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,ZETA2,
     & ZERO,ONE,HALF
      PARAMETER (ZERO =0.D0, ONE =1.D0, HALF=0.5D0)
      DATA A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,ZETA2/ -0.250000000000000D0,
     & -0.111111111111111D0,-0.010000000000000D0,-0.017006802721088D0,
     & -0.019444444444444D0,-0.020661157024793D0,-0.021417300648069D0,
     & -0.021948866377231D0,-0.022349233811171D0,-0.022663689135191D0,
     &  1.644934066848226D0/
      PROD(Y,Y2)=Y*(ONE+A1*Y*(ONE+A2*Y*(ONE+A3*Y2*(ONE+A4*Y2*(ONE+A5*Y2*
     & (ONE+A6*Y2*(ONE+A7*Y2*(ONE+A8*Y2*(ONE+A9*Y2*(ONE+A10*Y2))))))))))
      XR=REAL(X)
      XI=IMAG(X)
      R2=XR*XR+XI*XI
      IF (R2.GT.ONE.AND.(XR/R2).GT.HALF) THEN
         Z=-LOG(ONE/X)
         HWULI2=PROD(Z,Z*Z)+ZETA2-LOG(X)*LOG(ONE-X)+HALF*LOG(X)**2
      ELSEIF (R2.GT.ONE.AND.(XR/R2).LE.HALF) THEN
         Z=-LOG(ONE-ONE/X)
         HWULI2=-PROD(Z,Z*Z)-ZETA2-HALF*LOG(-X)**2
      ELSEIF (R2.EQ.ONE.AND.XI.EQ.ZERO) THEN
         HWULI2=ZETA2
      ELSEIF (R2.LE.ONE.AND.XR.GT.HALF) THEN
         Z=-LOG(X)
         HWULI2=-PROD(Z,Z*Z)+ZETA2-LOG(X)*LOG(ONE-X)
      ELSE
         Z=-LOG(ONE-X)
         HWULI2=PROD(Z,Z*Z)
      ENDIF
      END
