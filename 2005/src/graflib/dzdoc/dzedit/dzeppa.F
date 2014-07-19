*
* $Id: dzeppa.F,v 1.1.1.1 1996/03/04 16:13:24 mclareni Exp $
*
* $Log: dzeppa.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:24  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZEPPA(LUN,CPN,IXW,IYW)
      CHARACTER*80 CLINE
      COMMON/DZDPPC/CLINE
      CHARACTER*7 CPN
      INTEGER IFIRST
      DATA IFIRST/0/
      IF(IFIRST.EQ.0)THEN
         CLINE = ' '
         IFIRST = 1
         REWIND(LUN)
      ENDIF
      IXW = 0
      IYW = 0
      IF(CLINE(11:17).EQ.CPN)THEN
         GOTO 20
      ELSE
         REWIND(LUN)
      ENDIF
 
   10 CONTINUE
      CALL DZDCRD(LUN,CLINE,IRC)
      IF(IRC.NE.0)GOTO 30
*      READ(LUN,'(A)',END=30 )CLINE
      IF(CLINE(1:1).NE.'%')GOTO 10
      IF(CLINE(1:9).EQ.'%%PICTURE')THEN
         IF(CLINE(11:17).EQ.CPN)THEN
            GOTO 20
         ENDIF
      ENDIF
      GOTO 10
   20 CONTINUE
      READ(CLINE(18:27),'(2I5)')IXW,IYW
   30 CONTINUE
      END
***********************************************************************
