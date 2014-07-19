*
* $Id: dzdskd.F,v 1.1.1.1 1996/03/04 16:13:04 mclareni Exp $
*
* $Log: dzdskd.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:04  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDSKD
*      IMPLICIT NONE
#include "dzdzbrinc.inc"
#include "dzdprm.inc"
      CHARACTER*3 CYENO
      CHARACTER*12 CMD
      INTEGER NCH
      CALL DZDIBR
      CALL KUPATL(CMD,NPAR)
      IF(CMD.EQ.'BANKCOLOR')THEN
         CALL KUGETC(CYENO,NCH)
         IF(INDEX(CYENO,'Y').NE.0)THEN
            CCOL='C'
         ELSE
            CCOL=' '
         ENDIF
         CALL KUGETI(IBCCOL)
         CALL KUGETI(IBDCOL)
      ELSE IF(CMD.EQ.'GEOMFONT')THEN
         CALL KUGETR(GRIDSC)
         CALL KUGETI(IDZTFO)
         CALL KUGETI(IDZTPR)
      ELSE IF(CMD.EQ.'WORKSTATION')THEN
         CALL KUGETC(CYENO,NCH)
         IF(INDEX(CYENO,'Y').NE.0)THEN
            CCACT=' '
         ELSE
            CCACT='N'
         ENDIF
         CALL KUGETI(IWKZEB)
      ELSE IF(CMD.EQ.'REF_LINKS')THEN
         CALL KUGETC(CYENO,NCH)
         IF(INDEX(CYENO,'Y').NE.0)THEN
            IDREFL=1
         ELSE
            IDREFL=0
         ENDIF
      ELSE IF(CMD.EQ.'BROWSEKEY')THEN
         CALL KUGETI(IKDSP1)
         CALL KUGETI(IKDSP2)
         CALL KUGETI(ICYDSP)
      ELSE IF(CMD.EQ.'DZSHOW_OPT')THEN
         CALL KUGETC(CHOPD1,NCH)
      ELSE IF(CMD.EQ.'NEW_WINDOW')THEN
         CALL KUGETC(CYENO,NCH)
         IF(INDEX(CYENO,'Y').NE.0)THEN
            LNEWWI=.TRUE.
         ELSE
            LNEWWI=.FALSE.
         ENDIF
      ENDIF
      END
***********************************************************************
