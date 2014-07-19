*
* $Id: dot.F,v 1.1.1.1 1996/02/15 17:53:25 mclareni Exp $
*
* $Log: dot.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:25  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION DOT(X,Y)
C
      DIMENSION X(4),Y(4)
C
C
      N= 3
      GO TO 12
C
      ENTRY DOT4                                                         CDC
C     ENTRY DOT4 (X,Y)                                                  -CDC
      N=4
C
   12 A= 0
      DO 19 J=1,N
   19 A= A + X(J)*Y(J)
      DOT= A
C     DOT4= DOT                                                         IBM
      RETURN
      END
