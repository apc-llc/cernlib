*
* $Id: igpkpl.F,v 1.1.1.1 1996/02/14 13:10:37 mclareni Exp $
*
* $Log: igpkpl.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:37  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.14/09 26/05/92  13.57.05  by  O.Couet
*-- Author : O.Couet
      FUNCTION IGPKPL (XCUR,YCUR,N,XP,YP,DX,DY)
*.===========>
*.
*.   This routine decides if a given polyline is picked or not .
*. If picked IGPKPL=1
*. If not picked IGPKPL=0
*.
*. _Input parameters:
*.
*.  REAL XCUR : X coordinate of the cursor
*.  REAL YCUR : Y coordinate of the cursor
*.  INTEGER N : Number of point in the polyline
*.  REAL XP(N) YP(N) : Polyline coordinates
*.  REAL DX DY : Picking box. If a point is inside
*.               this box, it is picked
*.
*..==========> (O.Couet)
      DIMENSION XP(*),YP(*)
*.______________________________________
*
      NM=N-1
      IGPKPL=0
      BM=YCUR-DY
      BP=YCUR+DY
      AM=XCUR-DX
      AP=XCUR+DX
      DO 10 I=1,NM
         I1=I
         I2=I+1
*
*              Line segment parallel to oy
*
         IF(XP(I1).EQ.XP(I2))THEN
            IF((AM.LE.XP(I1)).AND.(XP(I2).LE.AP))THEN
               IF(YP(I1).LE.YP(I2))THEN
                  YI=YP(I1)
                  YIP=YP(I2)
               ELSE
                  YI=YP(I2)
                  YIP=YP(I1)
               ENDIF
               IF((YI.LE.YCUR).AND.(YCUR.LE.YIP))THEN
                  IGPKPL=1
                  RETURN
               ENDIF
            ENDIF
            GOTO 10
         ENDIF
*
*              Line segment paralel to ox
*
         IF(YP(I1).EQ.YP(I2))THEN
            IF((BM.LE.YP(I1)).AND.(YP(I1).LE.BP))THEN
               IF(XP(I1).LE.XP(I2))THEN
                  XI=XP(I1)
                  XIP=XP(I2)
               ELSE
                  XI=XP(I2)
                  XIP=XP(I1)
               ENDIF
               IF((XI.LE.XCUR).AND.(XCUR.LE.XIP))THEN
                  IGPKPL=1
                  RETURN
               ENDIF
            ENDIF
            GOTO 10
         ENDIF
*
*              Other line segment
*
         A=(YP(I1)-YP(I2))/(XP(I1)-XP(I2))
         B=(YP(I2)*XP(I1)-XP(I2)*YP(I1))/(XP(I1)-XP(I2))
         IF(XP(I1).LE.XP(I2))THEN
            XI=XP(I1)
            XIP=XP(I2)
         ELSE
            XI=XP(I2)
            XIP=XP(I1)
         ENDIF
         IF((XI.LE.XCUR).AND.(XCUR.LE.XIP))THEN
            AA=A*XCUR+B
            IF((BM.LE.AA).AND.(AA.LE.BP))THEN
               IGPKPL=1
               RETURN
            ENDIF
         ENDIF
         IF(YP(I1).LE.YP(I2))THEN
            YI=YP(I1)
            YIP=YP(I2)
         ELSE
            YI=YP(I2)
            YIP=YP(I1)
         ENDIF
         IF((YI.LE.YCUR).AND.(YCUR.LE.YIP))THEN
            BB=(YCUR-B)/A
            IF((AM.LE.BB).AND.(BB.LE.AP))THEN
               IGPKPL=1
               RETURN
            ENDIF
         ENDIF
   10 CONTINUE
*
      END
