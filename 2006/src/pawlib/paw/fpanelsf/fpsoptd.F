*
* $Id: fpsoptd.F,v 1.2 1996/05/31 16:07:03 couet Exp $
*
* $Log: fpsoptd.F,v $
* Revision 1.2  1996/05/31 16:07:03  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:10  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/09 16/08/95  16.33.24  by  O.Couet
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPSETOPTD(IOPTD)
*.===========> Author: G. Kozlovsky, 1994
*. Set D (weights) option
*..==========>
#include "hbook/hcfit2.inc"
      IDER = IOPTD
      END
