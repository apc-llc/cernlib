*
* $Id: fpsetnpar.F,v 1.3 1996/05/31 16:24:38 couet Exp $
*
* $Log: fpsetnpar.F,v $
* Revision 1.3  1996/05/31 16:24:38  couet
* - Cleaned up useless commented lines.
*
* Revision 1.2  1996/05/31 16:06:59  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:10  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 17/04/95  17.01.08  by  Gregory Kozlovsky
*-- Author :    Gregory Kozlovsky   17/04/95
      SUBROUTINE FPSETNPAR(NP)
      INTEGER NP
*.===========> Author: G. Kozlovsky, 1994
*.
*. Set user function number of parameters inside PAW
*..==========>
*
#include "hbook/hcfits.inc"
      NFPAR = NP
      END
