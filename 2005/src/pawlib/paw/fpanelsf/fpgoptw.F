*
* $Id: fpgoptw.F,v 1.2 1996/05/31 16:06:45 couet Exp $
*
* $Log: fpgoptw.F,v $
* Revision 1.2  1996/05/31 16:06:45  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:09  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/09 16/08/95  16.31.49  by  O.Couet
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPGETOPTW(IOPTW)
*.===========> Author: G. Kozlovsky, 1994
*. Set W (weights) option
*..==========>
#include "hbook/hcfit2.inc"
      IOPTW = IWEIGH
      END
