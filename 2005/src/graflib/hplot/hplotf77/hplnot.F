*
* $Id: hplnot.F,v 1.1.1.1 1996/01/19 10:50:09 mclareni Exp $
*
* $Log: hplnot.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:09  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/01 19/05/95  11.09.24  by  O.Couet
*-- Author :    O.Couet   10/05/95
      SUBROUTINE HPLNOT(XU,YU,DX,ISYM,CHTXT,CHOPT)
*.==========>
*.            Routine to anotate a plot (similar to HPLKEY but
*.            more complete). Cm are used for call to HPLTXT.
*..=========>
#include "hplot/hpl2.inc"
#include "hplot/hpl6.inc"
#include "hplot/hpl11.inc"
      CHARACTER*(*) CHTXT,CHOPT
      DIMENSION RTXFP(2)
      DIMENSION IOPT(4)
      EQUIVALENCE (IOPTF,IOPT(1)),(IOPTL,IOPT(2))
      EQUIVALENCE (IOPTW,IOPT(3)),(IOPTH,IOPT(4))
*._____________________________
*
      CALL UOPTC(CHOPT,'FLWH',IOPT)
*
      CALL IGQ('TXFP',RTXFP)
      CALL IGQ('FAIS',RFAIS)
      CALL IGQ('FASI',RFASI)
      CALL IGQ('LWID',RLWID)
      CALL IGQ('LTYP',RLTYP)
      CALL IGQ('BORD',RBORD)
      IF(NTWIN.NE.1)CALL ISELNT(1)
      NTWIN = 1
*----
*     Decode position information
*----
      XK    = FLOARG(XU)
      YK    = FLOARG(YU)
      KORNT = 1
      KFLG  = 1
      IF (IOPTF+IOPTH.NE.0) THEN
         IF (IOPTF.NE.0) THEN
            CALL ISFAIS(1)
            CALL ISFACI(ISYM)
         ENDIF
         IF (IOPTH.NE.0) THEN
            CALL ISFAIS(3)
            CALL ISFASI(ISYM)
         ENDIF
         CALL IGSET('LWID',1.)
         CALL ISPLCI(1)
         CALL IGSET('BORD',1.)
         CALL IGBOX(XK-DX,XK,YK-0.6*TVSIZ(5),YK+0.6*TVSIZ(5))
      ELSEIF (IOPTL+IOPTW.NE.0) THEN
         IF (IOPTL.NE.0) CALL ISLN(ISYM)
         IF (IOPTW.NE.0) CALL IGSET('LWID',FLOAT(ISYM))
         XLINE(1) = XK
         XLINE(2) = XK-DX
         YLINE(1) = YK
         YLINE(2) = YK
         CALL IPL(2,XLINE,YLINE)
      ELSE
         CALL HPLSYM(XK,YK,1,ISYM,0,'CM  ')
      ENDIF
*----
*     Calculate position for text
*----
      XK = XK+2.*TVSIZ(1)
      YK = YK-0.5*TVSIZ(5)
*---
*     Set font and precision
*----
      KFONT  = IABS(IHFONT(5))
      ITXFON = KFONT/10
      ITXPRE = MOD(KFONT,10)
      IF (IHFONT(5).LT.0) ITXFON = -ITXFON
      CALL ISTXFP(ITXFON,ITXPRE)
*
      CALL HPLTXT(XK,YK,CHTXT,5,KORNT,KFLG)
*
      CALL IGSET('FAIS',RFAIS)
      CALL IGSET('FASI',RFASI)
      CALL IGSET('LWID',RLWID)
      CALL IGSET('LTYP',RLTYP)
      CALL IGSET('BORD',RBORD)
      CALL ISTXFP(INT(RTXFP(1)),INT(RTXFP(2)))
      END
