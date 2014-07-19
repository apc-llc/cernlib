*
* $Id: d509hi.F,v 1.1.1.1 1996/02/15 17:48:47 mclareni Exp $
*
* $Log: d509hi.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:47  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
      SUBROUTINE D509HI(X,Y,X1,Y1,XSTEP,EPSI,M,MAXFUN,F,IS,A,B,Y4,Y5)
      EXTERNAL F
C
C IT GIVES A POINT X SUCH THAT Y IS LESS THAN Y1
C  IS=0  NORMAL, IS=1   M.GT.MAXFUN, IS=4  MINIMUM IS FOUND
C
      ETA=1.
      IS=0
      I=1
    1 X=X1+XSTEP
      DO 2 J=1,2
      M=M+1
      IF(M.GT.MAXFUN) GO TO 3
C
C COMPUTATION OF THE FUNCTION EITHER F, OR FA OR FB
C
      IF(X-A.GE.0.) GO TO 5
      Y4=F(A,I)
      Y=Y4+A-X
      GO TO 7
    5 IF(X-B.LE.0.) GO TO 6
      Y5=F(B,I)
      Y=Y5-B+X
      GO TO 7
    6 Y=F(X,I)
    7 CONTINUE
C
C END OF COMPUTATION OF Y
C
      IF(Y.LT.Y1) RETURN
    2 X=X1-XSTEP
      IF(XSTEP.LE.(ABS(X)+ETA)*EPSI) GO TO 4
      XSTEP=0.5*XSTEP
      GO TO 1
    4 IS=4
      X=X1
      Y=Y1
      RETURN
    3 IS=1
      RETURN
      END
