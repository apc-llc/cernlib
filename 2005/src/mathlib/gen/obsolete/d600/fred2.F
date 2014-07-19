*
* $Id: fred2.F,v 1.1.1.1 1996/04/01 15:03:14 mclareni Exp $
*
* $Log: fred2.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:14  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      FUNCTION FRED2(F,G,X)
      EXTERNAL F,G
      DIMENSION Z(96),W(96),V(96)
      COMMON /FREDCM/N,NFLAG,Z,W,V
C
C     ******************************************************************
C     FRED2 IS SET EQUAL TO Y(X), WHERE Y IS THE SOLUTION OF THE INTEG-
C     RAL EQUATION DEFINED BY A CALL TO FRED1.
C     ******************************************************************
C
C-----START
      IF(NFLAG.EQ.-4444) GO TO 1
      FRED2=0.0
      WRITE(6,200)
  200 FORMAT(' FRED2 ... HAS BEEN REFERENCED BEFORE FRED1 HAS'
     +        ,' BEEN CALLED')
      RETURN
C
    1 SUM=0.0
      DO 2 J=1,N
      SUM=SUM+W(J)*G( X, Z(J) )*V(J)
    2 CONTINUE
      FRED2=F(X)+SUM
      RETURN
      END
