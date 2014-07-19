*
* $Id: nzeros.F,v 1.1.1.1 1996/04/01 15:03:11 mclareni Exp $
*
* $Log: nzeros.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:11  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE NZEROS(NZ,Z,RR,FCN)
C
C     SEE PAPER OF LYNESS AND DELVES, MATH.OF COMP.21(1967),543-560.
C     RR IS THE RADIUS AND Z THE CENTRE OF THE REGION.
C
      DIMENSION F(512),A(512),RF(512)
C*NS  COMPLEX PP,Z0,Z,F,FCN,A,RF,T,U,S
      COMPLEX PP,Z0,Z,F,FCN,A,RF,T,U
      COMPLEX C2,C3
      EXTERNAL FCN
C
      NDIM=512
      P=6.28318530717958
      EPSI1=0.1
      AK=1.E07
      R=RR/1.2
      I=0
C
   20 CONTINUE
      ERROR3=1.E30
      N=16
      R=R*1.2
      NMIN=N-1
      PP=P*(0.,1.)/N
      DO 10 J=1,N
      Z0=Z+R*EXP(PP*J)
      F(J)=FCN(Z0,I)
      C1=ABS(F(J))
      IF(C1.LE.1.E-20) GO TO 20
   10 I=1
C
C     COMPUTES A(S) THE FIRST TIME
C
      DO 12 JS=1,NMIN
   12 A(JS)=0.
      DO 15 JS=1,NMIN
      DO 11 J=1,N
   11 A(JS)=EXP(-PP*JS*J)*F(J)+A(JS)
   15 A(JS)=A(JS)/N
      A(N)=0.
      DO 16 J=1,N
   16 A(N)=F(J)+A(N)
      A(N)=A(N)/N
C
C     DERIVATIVE RF THE FIRST TIME
C
      DO 13 J=1,N
   13 RF(J)=0.
      DO 14 J=1,N
      DO 14 JS=1,NMIN
   14 RF(J)=JS*A(JS)*EXP(PP*J*(JS-1))+RF(J)
C
C     INTEGRAL FOR THE NUMBER OF ROOTS
C
    1 T=0.
      U=0.
      PP=CMPLX(0.,P)/N
      N1=N/2
      DO 2 J=1,N1
      C2=RF(2*J)/F(2*J)
      C3=RF(2*J-1)/F(2*J-1)
      IF(ABS(C2).GT.AK.OR.ABS(C3).GT.AK) GO TO 20
      T=EXP(PP*2*J)*C2+T
      U=EXP(PP*(2*J-1))*C3+U
    2 CONTINUE
      T=T/N1
      U=U/N1
      ERROR1=ABS(T-U)*0.5
      IF(ERROR1.LE.EPSI1) GO TO 3
      IF(ERROR1.GT.ERROR3) GO TO 20
      ERROR3=ERROR1
      IF(N.LT.NDIM) GO TO 64
      WRITE(6,410)
  410 FORMAT( 5X,'CONVERGENCE IS TOO SLOW, CHANGE  R'//)
      RETURN
   64 CONTINUE
      CALL PREP(RF,F,A,N,Z,R,IP,FCN)
      IF(IP.EQ.1) GO TO 20
      N=2*N
      GO TO 1
    3 AB=ABS(REAL(0.5*T+0.5*U))
      NZ=NEAR1(AB)
      RETURN
      END
