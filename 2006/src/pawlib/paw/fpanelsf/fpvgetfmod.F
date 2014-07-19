*
* $Id: fpvgetfmod.F,v 1.3 1996/05/31 16:24:41 couet Exp $
*
* $Log: fpvgetfmod.F,v $
* Revision 1.3  1996/05/31 16:24:41  couet
* - Cleaned up useless commented lines.
*
* Revision 1.2  1996/05/31 16:07:07  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:10  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 09/04/95  23.16.46  by  Gregory Kozlovsky
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPVGETFMOD(USERFMOD)
      INTEGER USERFMOD
*.===========> Author: G. Kozlovsky, 1994
*.
*. Return 1 if log likelyhood fit model is set, 0 otherwise
*..==========>
*
#include "paw/fpcoms.inc"
*
      USERFMOD = INDEX(CHFOPTN,'Z')
      END
