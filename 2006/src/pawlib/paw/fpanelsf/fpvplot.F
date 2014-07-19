*
* $Id: fpvplot.F,v 1.2 1996/05/31 16:07:11 couet Exp $
*
* $Log: fpvplot.F,v $
* Revision 1.2  1996/05/31 16:07:11  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:10  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 18/04/95  12.07.04  by  O.Couet
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPVPLOT()
*
*.===========> Author: G. Kozlovsky, 1994
*.
*. Plot vectors without fit function (when function is not defined)
*..==========>
*
#include "hbook/hcbook.inc"
#include "hbook/hcfit6.inc"
#include "paw/fpcoms.inc"
*
      INTEGER LOGX,LOGY,LOGZ
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
* Draw the frame
      CALL HPLATT(IHPLATT)
      CALL HPLFRA(FPXMIN,FPXMAX,FPYMIN,FPYMAX,' ')
* Draw the vector
      CALL IGRAPH(NY,Q(ILXE),Q(ILYE),CHPOPTN)
*
* Update the picture
      CALL IGTERM

      END
