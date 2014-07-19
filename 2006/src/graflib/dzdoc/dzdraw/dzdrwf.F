*
* $Id: dzdrwf.F,v 1.1.1.1 1996/03/04 16:13:05 mclareni Exp $
*
* $Log: dzdrwf.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:05  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDRWF
*      IMPLICIT NONE
      INTEGER NCH, LUN
      CHARACTER*4 CHLUN
 
      CALL KUGETC(CHLUN,NCH)
      IF(NCH.LE.0)GOTO 999
      CALL DZDCTI(CHLUN,LUN)
      CALL FZENDI(LUN,'I')
999   END
***********************************************************************
