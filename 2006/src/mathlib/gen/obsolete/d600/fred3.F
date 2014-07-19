*
* $Id: fred3.F,v 1.1.1.1 1996/04/01 15:03:14 mclareni Exp $
*
* $Log: fred3.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:14  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      FUNCTION FRED3(H)
      EXTERNAL H
      DIMENSION Z(96),W(96),V(96)
      COMMON /FREDCM/N,NFLAG,Z,W,V
C
C     ******************************************************************
C     FRED3 = (H,Y) = INTEGRAL FROM A TO B OF H*Y WHERE Y IS THE SOLUT-
C     ION OF THE INTEGRAL EQUATION DEFINED BY A CALL TO FRED1.
C     ******************************************************************
C
C-----START
      IF(NFLAG.EQ.-4444) GO TO 1
      FRED3=0.0
      WRITE(6,200)
  200 FORMAT(' FRED3 ... HAS BEEN REFERENCED BEFORE FRED1 HAS'
     +        ,' BEEN CALLED')
      RETURN
C
    1 SUM=0.0
      DO 2 J=1,N
      SUM=SUM+W(J)*H(Z(J))*V(J)
    2 CONTINUE
      FRED3=SUM
      RETURN
      END
