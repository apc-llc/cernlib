*
* $Id: fpvgrupdt.F,v 1.3 1996/05/31 16:24:44 couet Exp $
*
* $Log: fpvgrupdt.F,v $
* Revision 1.3  1996/05/31 16:24:44  couet
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
*CMZ :  2.07/00 09/04/95  23.09.38  by  Gregory Kozlovsky
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPVGRUPDT(NPAR,UD,UFCN)
      DOUBLE PRECISION UD
      DIMENSION UD(*)
      EXTERNAL UFCN
*.===========> Author: G. Kozlovsky, 1994
*.
*. Update graphics for Vector Fit Panel
*..==========>
*
#include "hbook/hcbook.inc"
#include "hbook/hcfits.inc"
#include "hbook/hcfit6.inc"
#include "hbook/hcfitd.inc"
#include "paw/pawnpu.inc"
#include "paw/pcbuff.inc"
#include "paw/fpcoms.inc"
*
      INTEGER LOGX,LOGY,LOGZ
*
* Set current values of the function parameters
      DO 10 I=1,NPAR
         FITPAR(I) = UD(I)
         FITPAD(I) = UD(I)
   10 CONTINUE
*.___________________________________________
* Inquire if LOG scale are set
      CALL PAHLOG(LOGX,LOGY,LOGZ)
* If LOGY is ON CHOPT for IGRAPH should contain 'GY'
* If LOGX is ON CHOPT for IGRAPH should contain 'GX'
*
      IF (LOGX.NE.0.OR.LOGY.NE.0)CHPOPTN(32:32) = 'G'
      IF (LOGX.NE.0)CHPOPTN(31:31) = 'X'
      IF (LOGY.NE.0)CHPOPTN(30:30) = 'Y'
*
      X1 = Q(ILXE)
      XN = Q(ILXE+NY-1)
      DXF = (XN-X1)/FLOAT(NPFUNC)
      DO 50 I=1,NPFUNC
         XF = X1+(I-1)*DXF
         PAWBUF(1000+I) = XF
         IF(JAD.NE.0)THEN
            PAWBUF(I) = PAWFUN(XF)
         ELSE
            PAWBUF(I) = PAWSIM(XF)
         ENDIF
  50  CONTINUE
      FYMIN = VMIN(PAWBUF(1),NPFUNC)
      FYMAX = VMAX(PAWBUF(1),NPFUNC)
      DY=0.05*(FYMAX-FYMIN)
      IF(DY.EQ.0.)DY=0.05*FYMAX
      IF(DY.EQ.0.)DY=1.
      FYMIN = FYMIN-DY
      FYMAX = FYMAX+DY
      FYMIN = MIN(FPYMIN,FYMIN)
      FYMAX = MAX(FPYMAX,FYMAX)
* Draw the frame
      CALL HPLATT(IHPLATT)
      CALL HPLFRA(FPXMIN,FPXMAX,FYMIN,FYMAX,' ')
* Draw the vector
      CALL IGRAPH(NY,Q(ILXE),Q(ILYE),CHPOPTN)
* Draw fitted function
      CALL IGRAPH(NPFUNC,PAWBUF(1001),PAWBUF,'C'//CHPOPTN(30:32))
*
      CALL IGTERM
      END
