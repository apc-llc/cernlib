*
* $Id: hplax.F,v 1.2 1998/10/26 16:45:35 couet Exp $
*
* $Log: hplax.F,v $
* Revision 1.2  1998/10/26 16:45:35  couet
* - TVSIZ(3) was used instead of TVSIZ(4) to place the axis titles
*
* Revision 1.1.1.1  1996/01/19 10:49:57  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/00 19/04/95  10.26.37  by  O.Couet
*-- Author :
      SUBROUTINE HPLAX(CHXTIT,CHYTIT)
*.==========>
*. HPLOT routine to label axes with a title
*..=========>
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl4.inc"
#include "hplot/hpl6.inc"
#include "hplot/quest.inc"
      CHARACTER*(*) CHXTIT,CHYTIT
      DIMENSION RTXFP(2), RTXAL(2)
*._____________________________
*
*----
*     Return immediately if this routine is called before the
*     first HPLOT call
*----
      IF(IPLNUM.LT.0)THEN
         CALL HBUG('HPLAX must be called after HPLOT','HPLAX ',0)
         RETURN
      ENDIF
*
      NTWOLD = NTWIN
      IF (LO3DPL) THEN
         NTWIN = NTHIST
         CALL ISELNT(NTHIST)
         CALL HPLTOC(RQUEST(70),RQUEST(72),XKX1,YKX1,NT)
         CALL HPLTOC(RQUEST(71),RQUEST(73),XKX2,YKX2,NT)
         CALL HPLTOC(RQUEST(74),RQUEST(76),XKY1,YKY1,NT)
         CALL HPLTOC(RQUEST(75),RQUEST(77),XKY2,YKY2,NT)
         CALL HPLTOC(RQUEST(78),RQUEST(80),XKZ1,YKZ1,NT)
         CALL HPLTOC(RQUEST(79),RQUEST(81),XKZ2,YKZ2,NT)
      ENDIF
*----
*     Save the current NT and the current text angle and alignment.
*     Select the centimeters.
*----
      IF (NTWIN.NE.1) CALL ISELNT(1)
      NTWIN  = 1
      CALL IGQ('TANG',RTANG)
      CALL IGQ('TXAL',RTXAL)
      CALL IGQ('TXFP',RTXFP)
      CALL IGQ('CHHE',RCHHE)
*----
*     Set axis labels font and precision
*----
      KFONT                      = IABS(IHFONT(4))
      ILAFON                     = KFONT/10
      ILAPRE                     = MOD(KFONT,10)
      IF (IHFONT(4).LT.0) ILAFON = -ILAFON
      CALL ISTXFP(ILAFON,ILAPRE)
      CALL ISCHH(TVSIZ(4))
*----
*     Check if a Z title is required
*----
      IZPOS = INDEX(CHYTIT,';')
      IF (IZPOS.NE.0) THEN
         IYPOS = IZPOS-1
      ELSE
         IYPOS = LENOCC(CHYTIT)
      ENDIF
*----
*     Draw the axis title on a 3D plot
*----
      IF (LO3DPL) THEN
* X axis label
         IF     (XKX1.EQ.XKZ1) THEN
            XK = XKX1
            YK = YKX1
            CALL ISTXAL(0,0)
         ELSEIF (XKX2.EQ.XKZ1) THEN
            XK = XKX2
            YK = YKX2
            CALL ISTXAL(0,0)
         ELSE
            IF (XKX2.GT.XKX1) THEN
               XK = XKX2
               YK = YKX2
            ELSE
               XK = XKX1
               YK = YKX1
            ENDIF
            CALL ISTXAL(3,0)
         ENDIF
         IF (XKX2-XKX1.NE.0.) THEN
            RANG = (ATAN((YKX2-YKX1)/(XKX2-XKX1)))*(180./3.14159)
         ELSE
            RANG = 0.
         ENDIF
         CALL IGSET('TANG',RANG)
         CALL ITX(XK,YK-1.5*YLABEL-TVSIZ(4),CHXTIT)
* Y axis label
         IF     (XKY1.EQ.XKZ1) THEN
            XK = XKY1
            YK = YKY1
            CALL ISTXAL(0,0)
         ELSEIF (XKY2.EQ.XKZ1) THEN
            XK = XKY2
            YK = YKY2
            CALL ISTXAL(0,0)
         ELSE
            IF (XKY2.GT.XKY1) THEN
               XK = XKY2
               YK = YKY2
            ELSE
               XK = XKY1
               YK = YKY1
            ENDIF
            CALL ISTXAL(3,0)
         ENDIF
         IF (XKY2-XKY1.NE.0.) THEN
            RANG = (ATAN((YKY2-YKY1)/(XKY2-XKY1)))*(180./3.14159)
         ELSE
            RANG = 0.
         ENDIF
         CALL IGSET('TANG',RANG)
         CALL ITX(XK,YK-2.5*YLABEL-TVSIZ(4),CHYTIT(1:IYPOS))
* Z axis label
         IF(IZPOS.NE.0)THEN
            CALL ISTXAL(3,0)
            CALL IGSET('TANG',90.)
            CALL ITX(XKZ2-XLABEL,YKZ2,CHYTIT(IZPOS+1:))
         ENDIF
*----
*     Draw the axis title on a 2D plot
*----
      ELSE
         CALL ISTXAL(3,0)
* X axis label
         CALL IGSET('TANG',0.)
         XK = XHIGH
         YK = YLOW-YLABEL-TVSIZ(4)
         CALL ITX(XK,YK,CHXTIT)
* Y axis label
         CALL IGSET('TANG',90.)
         XK = XLOW-XLABEL
         YK = YHIGH
         CALL ITX(XK,YK,CHYTIT(1:IYPOS))
      ENDIF
*----
*     Restore the NT, the text angle and the text alignment.
*----
      IF(NTWIN.NE.NTWOLD)THEN
         NTWIN = NTWOLD
         CALL ISELNT(NTWOLD)
      ENDIF
      CALL IGSET('TANG',RTANG)
      CALL ISTXAL(INT(RTXAL(1)),INT(RTXAL(2)))
      CALL ISTXFP(INT(RTXFP(1)),INT(RTXFP(2)))
      CALL IGSET('CHHE',RCHHE)
*
      END
