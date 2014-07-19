*
* $Id: izpkpl.F,v 1.1.1.1 1996/02/14 13:10:24 mclareni Exp $
*
* $Log: izpkpl.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:24  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 16/12/88  14.00.51  by  O.Couet
*-- Author : O.Couet
      FUNCTION IZPKPL (XCUR,YCUR,N,XP,YP)
*.===========>
*.
*.   This routine decides if a given polyline is picked or not .
*. If picked IZPKPL=1
*. If not picked IZPKPL=0
*.
*. _Input parameters:
*.
*.  REAL XCUR : X coordinate of the cursor
*.  REAL YCUR : Y coordinate of the cursor
*.  INTEGER N : Number of point in the polyline
*.  REAL XP(N) YP(N) : Polyline coordinates
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hiatt.inc"
      DIMENSION XP(*),YP(*)
*.______________________________________
*
      S=1
      IF(REDIT.NE.0)THEN
         S=REDIT
      ENDIF
      DX=(S*0.01875*(RQUEST(21)-RQUEST(20)))/(RQUEST(31)-RQUEST(30))
      DY=(S*0.01875*(RQUEST(23)-RQUEST(22)))/(RQUEST(33)-RQUEST(32))
      NM=N-1
      IZPKPL=0
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
                  IZPKPL=1
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
                  IZPKPL=1
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
               IZPKPL=1
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
               IZPKPL=1
               RETURN
            ENDIF
         ENDIF
   10 CONTINUE
*
      END
