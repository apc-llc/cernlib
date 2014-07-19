*
* $Id: circle.F,v 1.1.1.1 1996/04/01 15:03:12 mclareni Exp $
*
* $Log: circle.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:12  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE CIRCLE(Z,E,F,B,N,M,MAXFUN)
C
C   THE SUB COMPUTES THE NEW POINT Z ON THE BOUNDARY OF THE SINGULAR
C   CIRCLE R(V,E). WHEN ENTERING Z=V. IN OUTPUT F(N+1) IS THE STARTING
C   FUNCTION AT Z FOR THE NEW HOMOTOPY.
C
      COMPLEX Z,F(1),B(1),V,ZV,Z1
      P=6.28318530717958
      V=Z
      K=2
      M1=1
C
    1 DT=P/K
      DO 2 J=1,K,M1
      TE=J*DT
      ZV=E*CMPLX(COS(TE),SIN(TE))
      Z=V+ZV
      IF(M.GE.MAXFUN) RETURN
      CALL VP(F,Z,B,1,JJ,N)
      M=M+1
      C=ABS(F(N))
      IF(C.EQ.0.) GO TO 2
      Z1=(C*F(N+1))/F(N)
      SCAL=REAL(Z1)*REAL(ZV)+AIMAG(Z1)*AIMAG(ZV)
      IF(SCAL.LT.0.) RETURN
    2 CONTINUE
      M1=2
      K=2*K
      IF(K.GT. 8) RETURN
      GO TO 1
      END
