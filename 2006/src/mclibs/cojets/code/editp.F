*
* $Id: editp.F,v 1.1.1.1 1996/01/11 14:14:35 mclareni Exp $
*
* $Log: editp.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:35  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE EDITP(N)
C     *******************
C-- ROTATES JETGEN OUTPUT
C-- REVISED: 88/04/27
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/decpar.inc"
#include "cojets/edpar.inc"
#include "cojets/itapes.inc"
#include "cojets/jet.inc"
      DIMENSION ROT(3,3), PR(3)
      IF(N.EQ.0) RETURN
C 2 ROTATE TO GIVE JET PRODUCED IN DIRECTION THETA, PHI
      IF(THETA.LT.1.E-10) RETURN
      CT=COS(THETA)
      ST=SIN(THETA)
      CP=COS(PHI)
      SP=SIN(PHI)
      ROT(1,1)=CT*CP
      ROT(1,2)=-SP
      ROT(1,3)=ST*CP
      ROT(2,1)=CT*SP
      ROT(2,2)=CP
      ROT(2,3)=ST*SP
      ROT(3,1)=-ST
      ROT(3,2)=0.
      ROT(3,3)=CT
      N1=N+1
      DO 120 J=1,3
120   P(N1,J)=PJTOT(J)
      DO 140 I=1,N1
      DO 130 J=1,3
  130 PR(J)=P(I,J)
      DO 140 J=1,3
  140 P(I,J)=ROT(J,1)*PR(1)+ROT(J,2)*PR(2)+ROT(J,3)*PR(3)
      DO 121 J=1,3
121   PJTOT(J)=P(N1,J)
      RETURN
      END
