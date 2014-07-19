*
* $Id: iprng.F,v 1.1.1.1 1996/02/14 13:11:07 mclareni Exp $
*
* $Log: iprng.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:07  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/00 01/11/93  10.47.48  by  O.Couet
*-- Author :    O.Couet   13/08/91
      SUBROUTINE IPRNG(XSIZI,YSIZI)
*.==========>
*.
*.  This routine sets the range for the paper in cm.
*.
*. _Input parameters:
*.
*.  REAL XSIZ YSIZ : Paper range.
*.
*..=========>
#include "higz/hiatt.inc"
#include "higz/hipost.inc"
*.______________________________________
*
      XSIZ=XSIZI
      YSIZ=YSIZI
*
      IF(IPSWTY.NE.-113)THEN
         XPS=XWKSIZ(IPSI)
         YPS=YWKSIZ(IPSI)
      ELSE
         XPS=XSIZ
         YPS=YSIZ
      ENDIF
      XPSR=XSIZ
      YPSR=YSIZ
*
      IF(XSIZ.LE.XPS.AND.YSIZ.LT.YPS)THEN
         XYMAX=MAX(XPS,YPS)
         XNCM=XSIZ/XYMAX
         YNCM=YSIZ/XYMAX
         DXWN=((XPS/XYMAX)-XNCM)/2.
         DYWN=((YPS/XYMAX)-YNCM)/2.
      ELSE
         XWKWN=MIN(1.,XPS/YPS)
         YWKWN=MIN(1.,YPS/XPS)
*
         IF (XSIZ.LT.YSIZ) THEN
            XNCM=YWKWN*XSIZ/YSIZ
            YNCM=YWKWN
            DXWN=(XWKWN-XNCM)/2.
            DYWN=0.
            IF(DXWN.LT.0.)THEN
               XNCM=XWKWN
               DXWN=0.
               YNCM=XWKWN*YSIZ/XSIZ
               DYWN=(YWKWN-YNCM)/2.
            ENDIF
         ELSE
            XNCM=XWKWN
            YNCM=XWKWN*YSIZ/XSIZ
            DXWN=0.
            DYWN=(YWKWN-YNCM)/2.
            IF(DYWN.LT.0.)THEN
               YNCM=YWKWN
               DYWN=0.
               XNCM=YWKWN*XSIZ/YSIZ
               DXWN=(XWKWN-XNCM)/2.
            ENDIF
         ENDIF
      ENDIF
      XVP1=DXWN
      XVP2=XNCM+DXWN
      YVP1=DYWN
      YVP2=YNCM+DYWN
      LOPRNG=.TRUE.
*
      END
