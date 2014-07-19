*
* $Id: mndxdi.F,v 1.1.1.1 1996/03/07 14:31:29 mclareni Exp $
*
* $Log: mndxdi.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:29  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNDXDI(PINT,IPAR,DXDI)
#include "minuit/d506dp.inc"
CC        calculates the transformation factor between external and
CC        internal parameter values.     this factor is one for
CC        parameters which are not limited.     called from MNEMAT.
#include "minuit/d506cm.inc"
      I = NEXOFI(IPAR)
      DXDI = 1.0
      IF (NVARL(I) .GT. 1)
     +      DXDI = 0.5 *ABS((BLIM(I)-ALIM(I)) * COS(PINT))
      RETURN
      END
