*
* $Id: fphplot.F,v 1.2 1996/05/31 16:06:51 couet Exp $
*
* $Log: fphplot.F,v $
* Revision 1.2  1996/05/31 16:06:51  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:10  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 18/04/95  12.07.58  by  O.Couet
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPHPLOT()
*.===========> Author: G. Kozlovsky, 1994
*.
*. Plot histogram without fit function (when function is not defined)
*..==========>
*
#include "paw/pawcom.inc"
#include "paw/pcrang.inc"
#include "paw/fpcoms.inc"
*.___________________________________________
*
      CALL PAPLOT(ID,' ',' ',0,ICRANG,ICX1,ICX2,ICY1,ICY2)
      CALL IGTERM
      END
