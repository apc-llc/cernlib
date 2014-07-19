*
* $Id: fphgetrng.F,v 1.3 1996/05/31 16:24:31 couet Exp $
*
* $Log: fphgetrng.F,v $
* Revision 1.3  1996/05/31 16:24:31  couet
* - Cleaned up useless commented lines.
*
* Revision 1.2  1996/05/31 16:06:47  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:09  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 09/04/95  22.43.03  by  Gregory Kozlovsky
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPHGETRNG(IX1,IX2,IY1,IY2)
      INTEGER IX1,IX2,IY1,IY2
*
*.===========> Author: G. Kozlovsky, 1994
*.
*. Get current histogram range which is set in PAW
*..==========>
*
#include "paw/pcrang.inc"
      IX1 = ICX1
      IX2 = ICX2
      IY1 = ICY1
      IY2 = ICY2
      END
