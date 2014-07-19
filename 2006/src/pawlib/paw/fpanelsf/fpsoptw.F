*
* $Id: fpsoptw.F,v 1.2 1996/05/31 16:07:05 couet Exp $
*
* $Log: fpsoptw.F,v $
* Revision 1.2  1996/05/31 16:07:05  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:10  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/09 16/08/95  16.33.24  by  O.Couet
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPSETOPTW(IOPTW)
*.===========> Author: G. Kozlovsky, 1994
*. Set W (weights) option
*. Note: for Vector Fit Panel this option is disabled because it
*.       is superseeded by explicit weigth vector EY specificication
*..==========>
#include "hbook/hcfit2.inc"
      IWEIGH = IOPTW
      END
