*
* $Id: fpsoptu.F,v 1.2 1996/05/31 16:07:04 couet Exp $
*
* $Log: fpsoptu.F,v $
* Revision 1.2  1996/05/31 16:07:04  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:10  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/09 16/08/95  16.33.24  by  O.Couet
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPSETOPTU(IOPTU)
*.===========> Author: G. Kozlovsky, 1994
*. Set U (weights) option
*..==========>
#include "hbook/hcfit2.inc"
      IFLFUN = IOPTU
      END
