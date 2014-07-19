*
* $Id: cfft.F,v 1.1.1.1 1996/02/15 17:48:48 mclareni Exp $
*
* $Log: cfft.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:48  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
      SUBROUTINE CFFT(A,MSIGN)
      COMPLEX A(1),U,W,T
      IF(MSIGN.EQ.0) RETURN
      M=IABS(MSIGN)
      N=2**M
      NV2=N/2
      NM1=N-1
      J=1
      DO 7 I=1,NM1
      IF(I.GE.J) GO TO 5
      T=A(J)
      A(J)=A(I)
      A(I)=T
 5    K=NV2
 6    IF(K.GE.J) GO TO 7
      J=J-K
      K=K/2
      GO TO 6
 7    J=J+K
      DO 8 I=1,N,2
      T=A(I+1)
      A(I+1)=A(I)-T
 8    A(I )=A(I)+T
      IF(M.EQ.1) RETURN
      C=0.
      S=ISIGN(1,MSIGN)
      LE=2
      DO 20 L=2,M
      W=CMPLX(C,S)
      U=W
      C=SQRT(C*.5+.5)
      S=AIMAG(W)/(C+C)
      LE1=LE
      LE=LE1+LE1
      DO 9 I=1,N,LE
      IP=I+LE1
      T=A(IP)
      A(IP)=A(I)-T
 9    A(I) =A(I)+T
      DO 20 J=2,LE1
      DO 10 I=J,N,LE
      IP=I+LE1
      T=A(IP)*U
      A(IP)=A(I)-T
 10   A(I) =A(I)+T
 20   U=U*W
      RETURN
      END
