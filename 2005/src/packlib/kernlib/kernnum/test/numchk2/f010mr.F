*
* $Id: f010mr.F,v 1.1.1.1 1996/02/15 17:48:42 mclareni Exp $
*
* $Log: f010mr.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:42  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
      REAL FUNCTION F010MR(M,N,IDIM,Q)
      REAL Q(IDIM,N)
C
C  F010MR IS SET EQUAL TO THE LARGEST ABSOLUTE VALUE OF ANY ELEMENT IN
C  THE MATRIX Q.
C     M     FIRST MATHEMATICAL DIMENSION OF Q.
C     N     SECOND MATHEMATICAL DIMENSION OF Q.
C     IDIM  FIRST DIMENSION OF ARRAY Q.
C     Q     (REAL) TWO-DIMENSIONAL ARRAY.
C
C  START.
      R=0.
      DO 2 I=1,M
         DO 1 J=1,N
            A= ABS(Q(I,J))
            R=AMAX1(R,A)
    1    CONTINUE
    2 CONTINUE
      F010MR=R
      RETURN
      END
