*
* $Id: fphgrupdt.F,v 1.3 1996/05/31 16:24:33 couet Exp $
*
* $Log: fphgrupdt.F,v $
* Revision 1.3  1996/05/31 16:24:33  couet
* - Cleaned up useless commented lines.
*
* Revision 1.2  1996/05/31 16:06:50  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:10  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 09/04/95  22.46.59  by  Gregory Kozlovsky
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPHGRUPDT(NPAR,UD,UFCN)
      DOUBLE PRECISION UD
      DIMENSION UD(*)
      EXTERNAL UFCN
*
*.===========> Author: G. Kozlovsky, 1994
*.
*. Update graphics for Histo Fit Panel
*..==========>
*
#include "hbook/hcfits.inc"
#include "hbook/hcfit6.inc"
#include "hbook/hcfitd.inc"
#include "hbook/hcbook.inc"
#include "paw/pawcom.inc"
#include "paw/pcrang.inc"
#include "paw/fpcoms.inc"
*.___________________________________________
*
      DO 10 I=1,NPAR
         FITPAR(I) = UD(I)
         FITPAD(I) = UD(I)
   10 CONTINUE
      CALL HFUNC(ID,UFCN)
      CALL PAPLOT(ID,' ',' ',0,ICRANG,ICX1,ICX2,ICY1,ICY2)
*
      CALL IGTERM
      END
