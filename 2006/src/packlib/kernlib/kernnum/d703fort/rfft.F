*
* $Id: rfft.F,v 1.1.1.1 1996/02/15 17:48:48 mclareni Exp $
*
* $Log: rfft.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:48  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
      SUBROUTINE RFFT(A,MSIGN)
      COMPLEX A(1),T1,T2,U,W
      IF(MSIGN.EQ.0) RETURN
      M=IABS(MSIGN)-1
      N=2**M
      U=(0.,1.)
      IF(MSIGN.GT.0) GO TO 2
      CALL CFFT(A,-M)
      F=.25/N
      DO 1 I=1,N
 1    A(I)=F*A(I)
      A(N+1)=A(1)
      U=CONJG(U)
 2    ANGL=3.1415926535898/ISIGN(N,MSIGN)
      W=CMPLX(COS(ANGL),SIN(ANGL))
      N2=N+2
      N1=N2/2
      DO 3 J=1,N1
      K=N2-J
      T1= A(J)+CONJG(A(K))
      T2=(A(J)-CONJG(A(K)))*U
      A(J)=      T1+T2
      A(K)=CONJG(T1-T2)
 3    U=U*W
      IF(MSIGN.GT.0) CALL CFFT(A, M)
      RETURN
      END
