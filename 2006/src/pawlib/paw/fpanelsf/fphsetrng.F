*
* $Id: fphsetrng.F,v 1.2 1996/05/31 16:06:54 couet Exp $
*
* $Log: fphsetrng.F,v $
* Revision 1.2  1996/05/31 16:06:54  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:10  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/03 20/06/95  14.37.11  by  O.Couet
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPHSETRNG(LOWBINS,UPPBINS)
*.===========>
*. Set current histogram range inside PAW
*..==========>
#include "paw/pcrang.inc"
      INTEGER LOWBINS(3),UPPBINS(3)
*
      ICX1   = LOWBINS(1)
      ICY1   = LOWBINS(2)
      ICX2   = UPPBINS(1)
      ICY2   = UPPBINS(2)
      ICRANG = 1
      END
