*
* $Id: deflat.F,v 1.1.1.1 1996/04/01 15:03:12 mclareni Exp $
*
* $Log: deflat.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:12  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE DEFLAT(A,N,Z)
C
C   N MUST BE NOT LESS THEN 2
C      GIVEN THE APPROXIMATE ZERO  Z  OF THE POLYNOMIAL  F(A,N)  IT
C     COMPUTES THE DEFLATED POL. F(A,N-1)  USING BACKWARD AND FOR
C     WARD DIVISION ACCORDING PETERS AND WILKINSON.  J. INST. MATHS.
C     APPLICS. (1971) 8,16-35.  N-1 IS PUT IN N.
C
      COMPLEX Z,A(1)
C
      JR=0
      IF(ABS(Z).EQ.0.) GO TO 3
C
C      COMPUTES THE MATCHING INDEX R
C
      X=ABS(A(N+1))
      DO 1 J=2,N
      Y=ABS(Z**(J-1)*A(N-J+2))
      IF(Y.LE.X) GO TO 1
      X=Y
      JR=J-1
    1 CONTINUE
C
C      BACKWARD DIVISION
C
      IF(JR.EQ.0) GO TO 3
      A(N+1)=-A(N+1)/Z
      IF(JR.EQ.1) GO TO 6
      N2=JR-1
      DO 2 J=1,N2
    2 A(N-J+1)=(A(N-J+2)-A(N-J+1))/Z
    6 DO 5 J=1,JR
    5 A(N-JR+J)=A(N-JR+J+1)
C
C     FORWARD DIVISION
C
    3 N1=N-JR
      N=N-1
      IF(N1.LT.2) RETURN
      DO 4 J=2,N1
    4 A(J)=A(J)+Z*A(J-1)
      RETURN
      END
