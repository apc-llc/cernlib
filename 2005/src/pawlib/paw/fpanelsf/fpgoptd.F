*
* $Id: fpgoptd.F,v 1.2 1996/05/31 16:06:43 couet Exp $
*
* $Log: fpgoptd.F,v $
* Revision 1.2  1996/05/31 16:06:43  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:09  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/09 16/08/95  16.25.47  by  O.Couet
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPGETOPTD(IOPTD)
*.===========> Author: G. Kozlovsky, 1994
*.
*. Set D (weights) option
*..==========>
#include "hbook/hcfit2.inc"
      IOPTD = IDER
      END
