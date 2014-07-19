*
* $Id: fphgrend.F,v 1.3 1996/05/31 16:24:32 couet Exp $
*
* $Log: fphgrend.F,v $
* Revision 1.3  1996/05/31 16:24:32  couet
* - Cleaned up useless commented lines.
*
* Revision 1.2  1996/05/31 16:06:48  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:09  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 09/04/95  22.49.01  by  Gregory Kozlovsky
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPHGREND
*.===========> Author: G. Kozlovsky, 1994
*.
*. End graphics for Histo Fit Panel
*..==========>
#include "hbook/hcbook.inc"
#include "paw/fpcoms.inc"
*
*            Draw fit parameters
*
      LCIDS = LCID
      LCID = 0
      CALL HPLFIT
      LCID = LCIDS
*
      CALL IGTERM
      END
