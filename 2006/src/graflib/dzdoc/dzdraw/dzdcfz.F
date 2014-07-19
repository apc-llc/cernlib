*
* $Id: dzdcfz.F,v 1.1.1.1 1996/03/04 16:13:05 mclareni Exp $
*
* $Log: dzdcfz.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:05  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDCFZ
*      IMPLICIT NONE
      INTEGER        INFLUN,INFSTA, INFOFZ(40)
      COMMON/FZSTAT/ INFLUN,INFSTA, INFOFZ
      INTEGER NCH, LUN
      CHARACTER*4 CHLUN
      LOGICAL BTEST
 
      CALL KUGETC(CHLUN,NCH)
      IF(NCH.LE.0)GOTO 999
      CALL DZDCTI(CHLUN,LUN)
      CALL FZINFO(LUN)
      IF(INFLUN.NE.LUN)THEN
         WRITE(*,*)'FZ file not open, LUN:',LUN
         GOTO 999
      ENDIF
      IF(BTEST(INFSTA,11))THEN
         CALL FZENDO(LUN,'T')
      ELSE
         CALL FZENDI(LUN,'T')
      ENDIF
      CLOSE(LUN)
999   END
***********************************************************************
