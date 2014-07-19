*
* $Id: fpsetpnam.F,v 1.3 1996/05/31 16:24:38 couet Exp $
*
* $Log: fpsetpnam.F,v $
* Revision 1.3  1996/05/31 16:24:38  couet
* - Cleaned up useless commented lines.
*
* Revision 1.2  1996/05/31 16:07:01  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:10  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 09/04/95  22.18.04  by  Gregory Kozlovsky
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPSETPNAM(PARNAME,IPAR)
      CHARACTER*(*) PARNAME
      INTEGER IPAR
*.===========> Author: G. Kozlovsky, 1994
*.
*. Set internal PAW parameter name
*..==========>
*
#include "hbook/hcfits.inc"
      FITNAM(IPAR) = PARNAME
      END
