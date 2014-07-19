*
* $Id: stabkp.F,v 1.1.1.1 1996/01/11 14:14:43 mclareni Exp $
*
* $Log: stabkp.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:43  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE STABKP(N)
C     ********************
C-- ELIMINATES UNSTABLE PARTICLES FROM P( , ) AND K( , )
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
#include "cojets/jet.inc"
C
      IF(N.EQ.0) RETURN
      I1=0
      DO 110 I=1,N
      IF(K(I,1).LE.0) GO TO 110
      I1=I1+1
      K(I1,1)=0
      K(I1,2)=K(I,2)
      DO 100 J=1,5
100   P(I1,J)=P(I,J)
110   CONTINUE
      N=I1
C
      RETURN
      END
