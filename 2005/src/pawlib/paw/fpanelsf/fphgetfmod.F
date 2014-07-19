*
* $Id: fphgetfmod.F,v 1.3 1996/05/31 16:24:30 couet Exp $
*
* $Log: fphgetfmod.F,v $
* Revision 1.3  1996/05/31 16:24:30  couet
* - Cleaned up useless commented lines.
*
* Revision 1.2  1996/05/31 16:06:46  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:09  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 09/04/95  22.50.03  by  Gregory Kozlovsky
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPHGETFMOD(LOGLMODE)
      INTEGER LOGLMODE
*.===========> Author: G. Kozlovsky, 1994
*.
*. Return 1 if log likelyhood fit model is set, 0 otherwise
*.
*..==========>
#include "hbook/hcfit2.inc"
      LOGLMODE = LINEAR
      END
