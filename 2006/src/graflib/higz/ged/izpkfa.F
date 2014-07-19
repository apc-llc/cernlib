*
* $Id: izpkfa.F,v 1.1.1.1 1996/02/14 13:10:24 mclareni Exp $
*
* $Log: izpkfa.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:24  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.14/09 21/05/92  14.57.26  by  O.Couet
*-- Author : O.Couet
      FUNCTION IZPKFA(XCUR,YCUR,N,XP,YP)
*.===========>
*.
*.   This function decides if a given fill area is picked or not .
*. If picked IZPKFA=1
*. If not picked IZPKFA=0
*.
*. _Input parameters:
*.
*.  REAL XCUR : X coordinate of the cursor
*.  REAL YCUR : Y coordinate of the cursor
*.  INTEGER N : Number of point in the fill area
*.  REAL XP(N) YP(N) : Fill area coordinates
*.
*..==========> (O.Couet)
      DIMENSION XLI(100),XP(*),YP(*)
*.______________________________________
*
      IZPKFA=0
      NBI=0
*
      DO 10 I=2,N+1
         I2=I
         I1=I-1
         IF(I.EQ.N+1)I2=1
*
*              Line segment parallel to oy
*
         IF(XP(I1).EQ.XP(I2))THEN
            IF(YP(I1).LT.YP(I2))THEN
               YI=YP(I1)
               YIP=YP(I2)
            ELSE
               YI=YP(I2)
               YIP=YP(I1)
            ENDIF
            IF((YI.LT.YCUR).AND.(YCUR.LT.YIP))THEN
               NBI=NBI+1
               XLI(NBI)=XP(I1)
            ENDIF
            GOTO 10
         ENDIF
*
*              Line segment parallel to ox
*
         IF(YP(I1).EQ.YP(I2))THEN
            IF(YP(I1).EQ.YCUR)THEN
               NBI=NBI+1
               XLI(NBI)=XP(I1)
               NBI=NBI+1
               XLI(NBI)=XP(I2)
            ENDIF
            GOTO 10
         ENDIF
*
*              Other line segment
*
         A=(YP(I1)-YP(I2))/(XP(I1)-XP(I2))
         B=(YP(I2)*XP(I1)-XP(I2)*YP(I1))/(XP(I1)-XP(I2))
         IF(XP(I1).LT.XP(I2))THEN
            XI=XP(I1)
            XIP=XP(I2)
         ELSE
            XI=XP(I2)
            XIP=XP(I1)
         ENDIF
         XIN=(YCUR-B)/A
         IF((XI.LT.XIN).AND.(XIN.LT.XIP))THEN
            NBI=NBI+1
            XLI(NBI)=XIN
         ENDIF
   10 CONTINUE
*
*              Sorting of the x coordinates intersections
*
      INV=0
      M=NBI-1
   20 CONTINUE
      DO 30 I=1,M
         IF(XLI(I+1).LT.XLI(I))THEN
            INV=INV+1
            RLL=XLI(I)
            XLI(I)=XLI(I+1)
            XLI(I+1)=RLL
         ENDIF
   30 CONTINUE
      M=M-1
      IF(INV.EQ.0)GOTO 40
      INV=0
      GOTO 20
*
*              Place of the x cursor coordinates
*
   40 NB=0
      DO 50 I=1,NBI
         IF(XCUR.LT.XLI(I))GOTO 60
         NB=NB+1
   50 CONTINUE
   60 IF(MOD(NB,2).NE.0)THEN
         IZPKFA=1
      ENDIF
*
      END
