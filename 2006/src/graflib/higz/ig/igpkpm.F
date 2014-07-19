*
* $Id: igpkpm.F,v 1.1.1.1 1996/02/14 13:10:37 mclareni Exp $
*
* $Log: igpkpm.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:37  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.14/09 25/05/92  17.43.32  by  O.Couet
*-- Author : O.Couet
      FUNCTION IGPKPM(XCUR,YCUR,N,XP,YP,DX,DY)
*.===========>
*.
*.   This function decides if a given polymarker is picked or not .
*. If picked IGPKPM=The picked point adress
*. If not picked IGPKPM=0
*.
*. _Input parameters:
*.
*.  REAL XCUR : X coordinate of the cursor
*.  REAL YCUR : Y coordinate of the cursor
*.  INTEGER N : Number of point in the polymarker
*.  REAL XP(N) YP(N) : Polymarker coordinates
*.  REAL DX DY : Picking box. If a point is inside
*.               this box, it is picked
*.
*..==========> (O.Couet)
      DIMENSION XP(*),YP(*)
*.______________________________________
*
      IGPKPM=0
*
      DO 10 I=1,N
         IF((ABS(XP(I)-XCUR).LE.DX).AND.(ABS(YP(I)-YCUR).LE.DY))THEN
            IGPKPM=I
            RETURN
         ENDIF
   10 CONTINUE
*
      END
