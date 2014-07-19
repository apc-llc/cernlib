*
* $Id: ikfntx.F,v 1.1.1.1 1996/02/14 13:10:55 mclareni Exp $
*
* $Log: ikfntx.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:55  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_X11)
*CMZ :  1.23/02 15/08/95  17.39.13  by  O.Couet
*-- Author :    O.Couet   15/03/91
      INTEGER FUNCTION IKFNTX(IWKNB)
*.===========>
*.
*. This routine manages the X11 hardware text fonts.
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#include "higz/hikern.inc"
#if !defined(CERNLIB_WINNT)
      PARAMETER (IFSIZE=7,IBIG=16)
      DIMENSION X11RAP(13)
      DATA X11RAP /1.000,1.000,1.010,0.910,0.920,0.920,0.925,1.204,
     +             1.204,1.168,1.166,1.007,1.026/
#endif
#if defined(CERNLIB_WINNT)
      DIMENSION IMSG(5)
      EQUIVALENCE (IMSG(1), Italic), (IMSG(2), IBOLD),
     +            (IMSG(3), IH)
#endif
*.______________________________________
*
      IKFNTX=0
      IF(RCHH.EQ.THX11.AND.IFPX11.EQ.ABS(IFONT))RETURN
*
      THX11  = RCHH
      IFPX11 = ABS(IFONT)
      FX11   = ' '
      IH     = INT(WKSC(IWKNB)*YRATIO*RCHH*1.5)
#if !defined(CERNLIB_WINNT)
      RSIZE  = FLOAT(IH)
      IF(IH.GT.40)IH=40
*
*              Set the X11 font name
*
#endif
#if (!defined(CERNLIB_IBM))&&(!defined(CERNLIB_WINNT))
      IF (IFPX11.EQ. 1) FX11='-*-times-medium-i-normal--'
      IF (IFPX11.EQ. 2) FX11='-*-times-bold-r-normal--'
      IF (IFPX11.EQ. 3) FX11='-*-times-bold-i-normal--'
      IF (IFPX11.EQ. 4) FX11='-*-helvetica-medium-r-normal--'
      IF (IFPX11.EQ. 5) FX11='-*-helvetica-medium-o-normal--'
      IF (IFPX11.EQ. 6) FX11='-*-helvetica-bold-r-normal--'
      IF (IFPX11.EQ. 7) FX11='-*-helvetica-bold-o-normal--'
      IF (IFPX11.EQ. 8) FX11='-*-courier-medium-r-normal--'
      IF (IFPX11.EQ. 9) FX11='-*-courier-medium-o-normal--'
      IF (IFPX11.EQ.10) FX11='-*-courier-bold-r-normal--'
      IF (IFPX11.EQ.11) FX11='-*-courier-bold-o-normal--'
      IF (IFPX11.EQ.12) FX11='-*-symbol-medium-r-normal--'
      IF (IFPX11.EQ.13) FX11='-*-times-medium-r-normal--'
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_WINNT))
      IF (IFPX11.EQ. 1) FX11='-*-courier-medium-o-normal--'
      IF (IFPX11.EQ. 2) FX11='-*-courier-bold-r-normal--'
      IF (IFPX11.EQ. 3) FX11='-*-courier-bold-o-normal--'
      IF (IFPX11.EQ. 4) FX11='-*-courier-medium-r-normal--'
      IF (IFPX11.EQ. 5) FX11='-*-courier-medium-o-normal--'
      IF (IFPX11.EQ. 6) FX11='-*-courier-bold-r-normal--'
      IF (IFPX11.EQ. 7) FX11='-*-courier-bold-o-normal--'
      IF (IFPX11.EQ. 8) FX11='-*-courier-medium-r-normal--'
      IF (IFPX11.EQ. 9) FX11='-*-courier-medium-o-normal--'
      IF (IFPX11.EQ.10) FX11='-*-courier-bold-r-normal--'
      IF (IFPX11.EQ.11) FX11='-*-courier-bold-o-normal--'
      IF (IFPX11.EQ.12) FX11='-*-symbol-medium-r-normal--'
      IF (IFPX11.EQ.13) FX11='-*-courier-medium-r-normal--'
#endif
#if !defined(CERNLIB_WINNT)
      IF(FX11.EQ.' ') RETURN
      IL=LENOCC(FX11)
*
*              Find the smallest size available.
*
      ISXFNT=IFSIZE
   10 CALL IZITOC(ISXFNT,FX11(IL+1:))
      IL2=LENOCC(FX11)+1
      FX11(IL2:IL2+1)='-*'
      IF(IXSETTF(0,IL2+1,FX11).NE.0)THEN
         IF(ISXFNT.EQ.IBIG)THEN
            CALL IGERR('X11 fonts not available','IKFNTX')
            RETURN
         ENDIF
         ISXFNT=ISXFNT+1
         GOTO 10
      ENDIF
*
*              Find the closest size available
*
   20 IF(IH.LT.ISXFNT)IH=ISXFNT
      CALL IZITOC(IH,FX11(IL+1:))
      IL2=LENOCC(FX11)+1
      FX11(IL2:IL2+1)='-*'
      ILL=IL2+1
      IF(IXSETTF(1,ILL,FX11).NE.0)THEN
         IF(IH.EQ.ISXFNT)THEN
            CALL IGERR('X11 fonts not available','IKFNTX')
            IKFNTX=1
            RETURN
         ENDIF
         IH=IH-1
         GOTO 20
      ENDIF
      IF(IPREC.EQ.2)THEN
         RMAGN = X11RAP(IFPX11)*RSIZE/FLOAT(IH)
      ELSE
         RMAGN = 1.
      ENDIF
*
#endif
#if defined(CERNLIB_WINNT)
*
*              Set the Windows/NT True Type text font.
*
      IF (IFPX11 .LE. 14) THEN
         GO TO (30 ,40 ,50 ,60 ,70 ,80 ,90 ,100,110,120 ,130 ,140 ,
     +   150, 160 ) IFPX11
         GO TO 100
   30    FX11 = 'Times New Roman'
         Italic = 1
         IBOLD = 4
         GO TO 170
   40    FX11 = 'Times New Roman'
         Italic = 0
         IBOLD = 8
         GO TO 170
   50    FX11 = 'Times New Roman'
         Italic = 1
         IBOLD = 8
         GO TO 170
   60    FX11 = 'Arial'
         Italic = 0
         IBOLD = 4
         GO TO 170
   70    FX11 = 'Arial'
         Italic = 1
         IBOLD = 4
         GO TO 170
   80    FX11 = 'Arial'
         Italic = 0
         IBOLD = 8
         GO TO 170
   90    FX11 = 'Arial'
         Italic = 1
         IBOLD = 8
         GO TO 170
  100    FX11 = 'Courier New'
         Italic = 0
         IBOLD = 4
         GO TO 170
  110    FX11 = 'Courier New'
         Italic = 1
         IBOLD = 4
         GO TO 170
  120    FX11 = 'Courier New'
         Italic = 0
         IBOLD = 8
         GO TO 170
  130    FX11 = 'Courier New'
         Italic = 1
         IBOLD = 8
         GO TO 170
  140    FX11 = 'Symbol'
         Italic = 0
         IBOLD = 6
         GO TO 170
  150    FX11 = 'Times New Roman'
         Italic = 0
         IBOLD = 4
         GO TO 170
  160    FX11 = 'Wingdings'
         Italic = 0
         IBOLD = 4
         GO TO 170
      ENDIF
  170 IMSG(4) = 10*RANGLE
      IMSG(5) = IPREC
      L       = LENOCC(FX11)
      IRET = IXSETTF(IMSG,L,FX11(:L)//CHAR(0))
#endif
      END
#endif
