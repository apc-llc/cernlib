*
* $Id: vp.F,v 1.1.1.1 1996/04/01 15:03:12 mclareni Exp $
*
* $Log: vp.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:12  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE VP(F,Z,A,K,J,N)
C
C   N IS THE DEGREE AND A ARE THE COEFFICIENTS OF THE POLYNOMIAL.
C   THE SUBROUTINE COMPUTES F WITH THE FIRST K DERIVATIVES DIVIDED
C   RESPECTIVELY BY K FACTORIAL.                      THEY ARE PUT
C   RESP. IN F(N+1), F(N),...  IN THE OUTPUT J=2 IF F CAN HAVE NO
C   SIGNIFICANT FIGURES, OTHERWISE J=1
C
      COMPLEX F(1),Z,A(1)
      COMMON /C206ER/ E
C
      C1=2.0000000000009
      C=ABS(Z)
      H=0.5*ABS(A(1))
      NPLUS=N+1
      M=NPLUS
      J=1
      DO 1 I=1,NPLUS
    1 F(I)=A(I)
C
C   F, BOUND E FOR THE ERROR (PETERS AND WILKINSON) AND EXIT
C
      DO 4 I=2,NPLUS
      F(I)=Z*F(I-1)+F(I)
      D=ABS(F(I))
    4 H=C*H+D
      E=1.E-14*(C1*H-D)
      IF(D.LE.E) J=2
      IF (K.EQ.0) RETURN
      IF(N.EQ.1) RETURN
C
C   DERIVATIVES
C
      IF(K.EQ.N) K=N-1
      DO 2 L=1,K
      M=M-1
      DO 3 I=2,M
    3 F(I)=Z*F(I-1)+F(I)
    2 CONTINUE
      RETURN
      END
