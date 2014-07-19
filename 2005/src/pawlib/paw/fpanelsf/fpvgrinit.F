*
* $Id: fpvgrinit.F,v 1.3 1996/05/31 16:24:43 couet Exp $
*
* $Log: fpvgrinit.F,v $
* Revision 1.3  1996/05/31 16:24:43  couet
* - Cleaned up useless commented lines.
*
* Revision 1.2  1996/05/31 16:07:10  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:10  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 09/04/95  23.32.10  by  Gregory Kozlovsky
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPVGRINIT
*.===========> Author: G. Kozlovsky, 1994
*.
*. Initialize graphics for Vector Fit Panel
*..==========>
*
#include "hbook/hcbook.inc"
#include "hbook/hcfit6.inc"
#include "paw/fpcoms.inc"
** Inquire if LOG scale are set
      CALL PAHLOG(LOGX,LOGY,LOGZ)
*
* Option 'S' is not valid for graphics update
      IOPTS=INDEX(CHPOPTN,'S')
      CHPOPTN(IOPTS:IOPTS)='?'
* Option 'B' is not valid for graphics update
      IOPTB=INDEX(CHPOPTN,'B')
      IF (IOPTB.NE.0)CHPOPTN(IOPTB:IOPTB) = ' '
* Option 'M' is not valid for graphics update
      IOPTM=INDEX(CHPOPTN,'M')
      IF (IOPTM.NE.0)CHPOPTN(IOPTM:IOPTM) = ' '

      YMIN=VMIN(Q(ILYE),NY)
      YMAX=VMAX(Q(ILYE),NY)
      DY=0.05*(YMAX-YMIN)
      IF(DY.EQ.0.)DY=0.05*YMAX
      IF(DY.EQ.0.)DY=1.
      XMIN=VMIN(Q(ILXE),NY)
      XMAX=VMAX(Q(ILXE),NY)
      DX=0.05*(XMAX-XMIN)
      IF(DX.EQ.0.)DX=0.05*XMAX
      IF(DX.EQ.0.)DX=1.
      X1=XMIN-DX
      X2=XMAX+DX
      Y1=YMIN-DY
      Y2=YMAX+DY
      IF(X1.LT.0..AND.XMIN.GE.0.)X1=0.
      IF(X2.GT.0..AND.XMAX.LE.0.)X2=0.
      IF(Y1.LT.0..AND.YMIN.GE.0.)Y1=0.
      IF(Y2.GT.0..AND.YMAX.LE.0.)Y2=0.
      IF(LOGX.NE.0)THEN
         X1=0.5*XMIN
         X2=2.*XMAX
      ENDIF
      IF(LOGY.NE.0)THEN
         Y1=0.5*YMIN
         Y2=2.*YMAX
      ENDIF
      FPXMIN = X1
      FPXMAX = X2
      FPYMIN = Y1
      FPYMAX = Y2

      END
