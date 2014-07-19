*
* $Id: d203m.F,v 1.1.1.1 1996/04/01 15:01:24 mclareni Exp $
*
* $Log: d203m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:24  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D203M
C     This program tests the operation of MATHLIB subprograms
C     RKNYS and DRKNYS (D(203)

#include "gen/imp64.inc"
C     Set maximum error allowed for test to be considered successful
      PARAMETER ( TSTERR=1D-6 )

      EXTERNAL SD203

      DIMENSION Y(2),YP(2),U(2),UP(2),R(2),RP(2),W(12)

      PARAMETER (Z0 = 0, Z1 = 1)
      PARAMETER (A1 = -8, A2 = 7*Z1/3, A3 = 5, A4 = -7*Z1/6)
#include "iorc.inc"

      CALL HEADER('D203',0)
      ERRMAX=0D0

      WRITE(LOUT,'(/6X,''X'',13X,''Y(1)'',16X,''Y(2)'',16X,
     +''YP(1)'',16X,''YP(2)'',8X,
     +''ErY1'',6X,''ErY2'',6X,''ErYP1'',6X,''ErYP2'')')
      X=0
      Y(1)=0
      Y(2)=0
      YP(1)=0
      YP(2)=0
      WRITE(LOUT,'(1X,F8.3,4F20.12)') X,Y(1),Y(2),YP(1),YP(2)
      DO 1 J = 0,99
#if defined(CERNLIB_DOUBLE)
      CALL DRKNYS(2,Z1/200,X,Y,YP,SD203,W)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL  RKNYS(2,Z1/200,X,Y,YP,SD203,W)
#endif
      S1=SIN(X)
      S2=SIN(2*X)
      C1=COS(X)
      C2=COS(2*X)
      U(1)=A1*C1+A2*S1+A3*C2+A4*S2+3
      U(2)=5*(A1*S1-A2*C1)+4*(A3*S2-A4*C2)+7
      UP(1)=-(A1*S1-A2*C1+2*A3*S2-2*A4*C2)
      UP(2)=5*(A1*C1+A2*S1)+8*(A3*C2+A4*S2)
      DO 2 I = 1,2
      R(I)=ABS((Y(I)-U(I))/U(I))
         ERRMAX= MAX(ERRMAX,R(I))
      RP(I)=ABS((YP(I)-UP(I))/UP(I))
         ERRMAX= MAX(ERRMAX,RP(I))
    2 CONTINUE
      WRITE(LOUT,'(1X,F8.3,4F20.12,4D10.1)')
     1 X,Y(1),Y(2),YP(1),YP(2),R(1),R(2),RP(1),RP(2)
    1 CONTINUE
      WRITE(LOUT,'(/'' Largest Relative Error was'',D10.1)') ERRMAX
C     Check if the test was successful
      IRC=ITEST('D203',ERRMAX .LE. TSTERR)
      CALL PAGEND('D203')
      RETURN
      END

      SUBROUTINE SD203(X,Y,YP,F)
#include "gen/imp64.inc"

      DIMENSION Y(*),YP(*),F(*)

      F(1)=4*Y(1)-YP(2)-12
      F(2)=Y(2)+10*YP(1)-7

      RETURN
      END
