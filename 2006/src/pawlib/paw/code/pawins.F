*
* $Id: pawins.F,v 1.1.1.1 1996/03/01 11:38:43 mclareni Exp $
*
* $Log: pawins.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  1.05/01 16/12/88  17.11.21  by  Rene Brun
*-- Author :    Rene Brun   03/01/89
      FUNCTION PAWINS(N,XV,YV,X,Y)
*.
*.         Function which returns 1. if point X,Y lies inside the
*.              polygon defined by the N points XV,YV
*.                                0 otherwise
*.
*.     The loop is executed with the end-point coordinates of a
*.     line segment (X1,Y1)-(X2,Y2) and the Y-coordinate of a
*.     horizontal line.
*.     J is incremented if the line (X1,Y1)-(X2,Y2)
*.     intersects the horizontal line.
*.     In this case XINT is set to the X-coordinate of the
*.     intersection point.
*.     If J is an odd number, then the point X,Y is not within
*.     the polygon.
*.
*.         This routine is based on an original algorithm
*.         developed by R.Nierhaus.
*.
      DIMENSION XV(*),YV(*)
C.
C.    ------------------------------------------------------------------
C.
      J=0
      DO 10 I=1,N
         IF(YV(I).EQ.YV(I+1))GO TO 10
         IF(Y.LT.YV(I).AND.Y.LT.YV(I+1))GO TO 10
         IF(YV(I).LT.Y.AND.YV(I+1).LT.Y)GO TO 10
         XINT=XV(I)+(Y-YV(I))*(XV(I+1)-XV(I))/(YV(I+1)-YV(I))
         IF(X.LT.XINT)J=J+1
  10  CONTINUE
*
      IF(J.NE.2*(J/2))THEN
         PAWINS=1.
      ELSE
         PAWINS=0.
      ENDIF
      END
