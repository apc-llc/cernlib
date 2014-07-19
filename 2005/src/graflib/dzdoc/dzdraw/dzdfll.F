*
* $Id: dzdfll.F,v 1.1.1.1 1996/03/04 16:13:05 mclareni Exp $
*
* $Log: dzdfll.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:05  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDFLL
*      IMPLICIT NONE
      INTEGER NCH, LUN, LOGL
      CHARACTER*4 CHLUN
 
      CALL KUGETC(CHLUN,NCH)
      IF(NCH.LE.0)GOTO 999
      CALL DZDCTI(CHLUN,LUN)
      CALL KUGETI(LOGL)
      CALL FZLOGL(LUN,LOGL)
999   END
***********************************************************************
