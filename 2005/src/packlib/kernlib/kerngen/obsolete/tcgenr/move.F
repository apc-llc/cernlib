*
* $Id: move.F,v 1.1.1.1 1996/02/15 17:53:27 mclareni Exp $
*
* $Log: move.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:27  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE MOVE (A,B,N)
C
      DIMENSION A(2),B(2)
C
C
      IF (N) 10,99,20
C
C--                BEGINNING OF B ON END OF A
C
   10 J= -N
   11 B(J)= A(J)
      J= J-1
      IF (J.NE.0) GO TO 11
      RETURN
C
C--                NO OVERLAP  OR  BEGINNING OF A ON END OF B
C
   20   DO 21 I=1,N
   21 B(I)= A(I)
   99 RETURN
      END
