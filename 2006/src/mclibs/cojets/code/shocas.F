*
* $Id: shocas.F,v 1.1.1.1 1996/01/11 14:14:42 mclareni Exp $
*
* $Log: shocas.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:42  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE SHOCAS(N)
C     ********************
C-- LISTS CASCADE PARTICLES
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/data2.inc"
#include "cojets/data3.inc"
#include "cojets/decpar.inc"
#include "cojets/itapes.inc"
#include "cojets/jet.inc"
      CHARACTER*5 C1,C2,C3
      WRITE(ITLIS,110)
      IF(N.EQ.0) RETURN
      DO 100 I=1,N
      C2=CHA2(K(I,2))
      C3=CHA3(1)
      IF(IDB(K(I,2)).EQ.0) C3=CHA3(2)
100   WRITE(ITLIS,130) I,K(I,1),C2,C3,(P(I,J),J=1,5)
      RETURN
110   FORMAT(//T11,1HI,T17,3HORI,T24,4HPART,T32,4HSTAB,
     *T44,2HPX,T56,2HPY,T68,2HPZ,T80,1HE,T92,1HM/)
130   FORMAT(8X,I4,2X,I5,2(4X,A4),5(3X,E9.3))
      END
