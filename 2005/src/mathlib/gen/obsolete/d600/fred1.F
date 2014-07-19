*
* $Id: fred1.F,v 1.1.1.1 1996/04/01 15:03:13 mclareni Exp $
*
* $Log: fred1.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:13  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE FRED1(F,G,A,B,N,Q,NQDIM)
      EXTERNAL F,G
      DIMENSION Q(NQDIM,2),Z(96),W(96),V(96),R(96)
      COMMON /FREDCM/NCOM,NFLAG,Z,W,V
C
C     ******************************************************************
C     PRELIMINARY CALCULATIONS FOR SOLVING THE INTEGRAL EQUATION
C
C                  Y(X) = F(X) + ( G(X,T), Y(T) )
C
C     WHERE THE LAST TERM (G,Y) DENOTES AN INTEGRAL OVER T FROM A TO B.
C
C     WITHIN THE PROGRAM Z HOLDS THE GAUSS ZEROS, W HOLDS THE GAUSS
C     WEIGHTS, AND V HOLDS THE VALUES OF Y CALCULATED AT Z.
C     ******************************************************************
C
C-----START
      NCOM=N
      NFLAG=-4444
      CALL GSET(A,B,NCOM,Z,W)
C
      DO 1 I=1,NCOM
      DO 2 J=1,NCOM
      Q(I,J)=-W(J)*G( Z(I), Z(J) )
    2 CONTINUE
      Q(I,I)=1.0+Q(I,I)
      V(I)=F(Z(I))
    1 CONTINUE
      CALL REQN(NCOM,Q,NQDIM,R,IFAIL,1,V)
      RETURN
      END
