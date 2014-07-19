*
* $Id: prep.F,v 1.1.1.1 1996/04/01 15:03:11 mclareni Exp $
*
* $Log: prep.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:11  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE PREP(RF,F,A,N,Z,R,IP,FCN)
C     SEE PAPER OF LYNESS AND DELVES. RF,F,A  ARE THE 2*N VALUES OF
C     R*DERIVATIVE,        OF THE FUNCTION F,  AND OF AS.  N VALUES
C       ARE KNOWN AND 2*N VALUES ARE OBTAINED. Z IS THE CENTRE AND R THE
C     RADIUS OF THE CIRCULAR REGION.
C
      DIMENSION F(512),B(512),A(512),DUM(512),RF(512)
      COMPLEX P,F,Z,Z0,B,A,DUM,RF
      EXTERNAL FCN
      COMPLEX FCN
C
      M=2*N
      P=6.28318530717958*(0.,1.)/M
      DO 1 J=1,N
    1 DUM(J)=F(J)
      DO 8 J=1,N
    8 F(2*J)=DUM(J)
      DO 2 J=1,N
      Z0=Z+R*EXP(P*(2*J-1))
      F(2*J-1)=FCN(Z0,I)
      C1=ABS(F(2*J-1))
      IF(C1.GT.1.E-20) GO TO 2
      IP=1
      RETURN
    2 CONTINUE
      DO 4 J=1,N
      B(J)=F(1)*EXP(-P*J)
      DO 3 K=2,N
    3 B(J)=F(2*K-1)*EXP(-P*J*(2*K-1))+B(J)
      B(J)=B(J)/N
      A(J+N)=0.5*(A(J)-B(J))
    4 A(J)=0.5*(A(J)+B(J))
C
C     COMPUTES THE VALUES OF THE DERIVATIVE
C
      DO 5 J=1,N
    5 DUM(J)=RF(J)
      DO 6 J=1,N
      RF(2*J)=A(N)*EXP(-P*2*J)
      RF(2*J-1)=-N*A(N)*EXP(-P*(2*J-1))
      DO 7 K=2,N
      RF(2*J)=A(K-1+N)*EXP(P*2*J*(K-2))+RF(2*J)
      RF(2*J-1)=((K-1)*B(K-1)-N*A(K-1+N))*EXP(P*
     1(2*J-1)*(K-2))+RF(2*J-1)
    7 CONTINUE
    6 RF(2*J)=RF(2*J)*N+DUM(J)
      RETURN
      END
