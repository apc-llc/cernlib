*
* $Id: ighatc.F,v 1.1.1.1 1996/02/14 13:10:34 mclareni Exp $
*
* $Log: ighatc.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:34  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/06 24/10/91  10.46.02  by  O.Couet
*-- Author :
      SUBROUTINE IGHATC(DY,ANGLE,N,XP,YP)
*.===========>
*.
*.    This routine draw hatches inclined with the
*. angle ANGLE and spaced of DY in normalized device
*. coordinates in the surface defined by N,XP,YP.
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
      PARAMETER (PI = 3.1415926,EPSIL=0.0001)
      DIMENSION XLI(100),XLH(2),YLH(2),XP(N),YP(N),XT(2),YT(2)
      DOUBLE PRECISION XT, YT, A, B, XI, XIP, XIN
      REAL LL
      LOGICAL ZSAV
*.___________________________________________
*
      RATIO1=(RVXMAX-RVXMIN)/(RWXMAX-RWXMIN)
      RATIO2=(RVYMAX-RVYMIN)/(RWYMAX-RWYMIN)
      RVX=RVXMIN
      RVY=RVYMIN
      RVX2=RVXMAX
      RVY2=RVYMAX
      RWX=RWXMIN
      RWY=RWYMIN
*
      IPLCIS=IPLCI
      ILNS=ILN
      RLWSAV=RLWSC
      CALL ISPLCI(IFACI)
      CALL ISLN(1)
      CALL ISLWSC(1.)
      ZSAV=ZFLAG
      ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
*
      ANGR=PI*(180-ANGLE)/180.
      SINA=SIN(ANGR)
      COSA=COS(ANGR)
      IF(ABS(COSA).LE.EPSIL)COSA=0.
      IF(ABS(SINA).LE.EPSIL)SINA=0.
      SINB=-SINA
      COSB=COSA
*
*              Search YMIN and YMAX
*
      YMIN=1.
      YMAX=0.
      DO 10 I=1,N
         X=RATIO1*(XP(I)-RWXMIN)+RVXMIN
         Y=RATIO2*(YP(I)-RWYMIN)+RVYMIN
         YROT=SINA*X+COSA*Y
         IF(YROT.GT.YMAX)YMAX=YROT
         IF(YROT.LT.YMIN)YMIN=YROT
   10 CONTINUE
      YMAX=FLOAT(INT(YMAX/DY))*DY
*
      NTSAV=INTR
      CALL ISELNT(0)
*
      DO 70 YCUR=YMAX,YMIN,-DY
         NBI=0
         DO 20 I=2,N+1
            I2=I
            I1=I-1
            IF(I.EQ.N+1)I2=1
            X1=RATIO1*(XP(I1)-RWX)+RVX
            Y1=RATIO2*(YP(I1)-RWY)+RVY
            X2=RATIO1*(XP(I2)-RWX)+RVX
            Y2=RATIO2*(YP(I2)-RWY)+RVY
            XT(1)=COSA*X1-SINA*Y1
            YT(1)=SINA*X1+COSA*Y1
            XT(2)=COSA*X2-SINA*Y2
            YT(2)=SINA*X2+COSA*Y2
*
*              Line segment parallel to oy
*
            IF(XT(1).EQ.XT(2))THEN
               IF(YT(1).LT.YT(2))THEN
                  YI=YT(1)
                  YIP=YT(2)
               ELSE
                  YI=YT(2)
                  YIP=YT(1)
               ENDIF
               IF((YI.LE.YCUR).AND.(YCUR.LT.YIP))THEN
                  NBI=NBI+1
                  XLI(NBI)=XT(1)
               ENDIF
               GOTO 20
            ENDIF
*
*              Line segment parallel to ox
*
            IF(YT(1).EQ.YT(2))THEN
               IF(YT(1).EQ.YCUR)THEN
                  NBI=NBI+1
                  XLI(NBI)=XT(1)
                  NBI=NBI+1
                  XLI(NBI)=XT(2)
               ENDIF
               GOTO 20
            ENDIF
*
*              Other line segment
*
            A=(YT(1)-YT(2))/(XT(1)-XT(2))
            B=(YT(2)*XT(1)-XT(2)*YT(1))/(XT(1)-XT(2))
            IF(XT(1).LT.XT(2))THEN
               XI=XT(1)
               XIP=XT(2)
            ELSE
               XI=XT(2)
               XIP=XT(1)
            ENDIF
            XIN=(YCUR-B)/A
            IF ((XI.LE.XIN).AND.(XIN.LT.XIP).AND.
     +          (MIN(YT(1),YT(2)).LE.YCUR).AND.
     +          (YCUR.LT.MAX(YT(1),YT(2))))THEN
               NBI=NBI+1
               XLI(NBI)=XIN
            ENDIF
   20    CONTINUE
*
*              Sorting of the x coordinates intersections
*
         INV=0
         M=NBI-1
   30    CONTINUE
         DO 40 I=1,M
            IF(XLI(I+1).LT.XLI(I))THEN
               INV=INV+1
               LL=XLI(I)
               XLI(I)=XLI(I+1)
               XLI(I+1)=LL
            ENDIF
   40    CONTINUE
         M=M-1
         IF(INV.EQ.0)GOTO 50
         INV=0
         GOTO 30
*
*              Draw the hatches
*
   50    IF(MOD(NBI,2).NE.0)GOTO 70
*
         DO 60 I=1,NBI,2
            XLH(1)=COSB*XLI(I)-SINB*YCUR
            YLH(1)=SINB*XLI(I)+COSB*YCUR
            XLH(2)=COSB*XLI(I+1)-SINB*YCUR
            YLH(2)=SINB*XLI(I+1)+COSB*YCUR
*
* Clipping on the current viewport
*
            IF(IGCLIP(XLH,YLH,RVX,RVX2,RVY,RVY2).NE.0)THEN
               CALL IPL(2,XLH,YLH)
            ENDIF
   60    CONTINUE
   70 CONTINUE
*
      CALL ISELNT(NTSAV)
      CALL ISPLCI(IPLCIS)
      CALL ISLN(ILNS)
      CALL ISLWSC(RLWSAV)
      ZFLAG=ZSAV
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      END
