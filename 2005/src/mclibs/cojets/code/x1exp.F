*
* $Id: x1exp.F,v 1.1.1.1 1996/01/11 14:14:44 mclareni Exp $
*
* $Log: x1exp.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:44  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION X1EXP(A)
C     *****************
C--RETURNS X WHERE DISTRIB. IN X IS GIVEN BY
C--         F(X) = X*EXP(-X)
C--WHERE A IS MIN. VALUE OF X, MAX. ASSUMED INFINITY.
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      DIMENSION F(100),X(100)
      DATA AP/0./
      IF(A.EQ.AP) GO TO 150
C--ON FIRST CALL SET UP TABLE
      DO 100 I=1,100
      X(I)=0.2*(FLOAT(I)-1.)+A
      F(I)=EXP(A-X(I))*(1.+X(I))/(1.+A)
100   CONTINUE
C--
150   CONTINUE
      R=CJRN(XXX)
      DO 300 I=1,100
      IF(R.GT.F(I)) GO TO 400
300   CONTINUE
400   CONTINUE
      IF(I.GT.1) GO TO 450
      X1EXP=0.
      AP=A
      RETURN
450   CONTINUE
      X1EXP=X(I-1)+0.2*(R-F(I-1))/(F(I)-F(I-1))
      RETURN
      END
