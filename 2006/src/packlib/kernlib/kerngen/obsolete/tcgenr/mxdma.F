*
* $Id: mxdma.F,v 1.1.1.1 1996/02/15 17:53:27 mclareni Exp $
*
* $Log: mxdma.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:27  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE MXDMA(A,F,D,B,I)
C
C
      DIMENSION A(400),B(400),D(20)
C
      IF (I.EQ.0) GO TO 100
      J=1
      IF (I.EQ.1) GO TO 12
      DO 11 K=2,I
      B(J)=A(J)+F*D(K-1)
      J=J+1
      DO 11 L=1,I
      B(J)=A(J)
   11 J=J+1
   12 B(J)=A(J)+F*D(I)
  100 RETURN
      END
