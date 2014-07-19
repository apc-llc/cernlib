*
* $Id: dzeop1.F,v 1.1.1.1 1996/03/04 16:13:22 mclareni Exp $
*
* $Log: dzeop1.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:22  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZEOP1(CHOPTO,ISTAT)
      CHARACTER*(*) CHOPTO
      CHARACTER*4 CDUMMY
*     open RZ file
#include "dzeditkeep.inc"
      IF(LUNRZ.EQ.0)THEN
         LRECL = 256
         IF(INDEX(CHOPTO,'U').EQ.0)THEN
            IRWRZF = 1
         ELSE
            IRWRZF = 2
         ENDIF
         LUNRZ=LUPRZ
         CALL RZOPEN(LUNRZ,CDUMMY,CHRZF,CHOPTO,LRECL,ISTAT)
         IF(ISTAT.NE. 0)THEN
            WRITE(*,*)'Error opening ',CHRZF
            LUNRZ=0
            GOTO 10
         ENDIF
         CALL DZDINI
         CHDZED = '//RZDOC'
         CALL RZFILE(LUNRZ,CHDZED(3:),CHOPTO)
         IF(CHSUBD .NE. ' ')THEN
            CALL RZCDIR(CHSUBD,' ')
         ENDIF
*         CALL RZLOGL(LUNRZ,2)
      ELSE
         WRITE(*,*)' ',CHRZF(1:LNBLNK(CHRZF)), ' already open '
      ENDIF
   10 CONTINUE
      END
***********************************************************************
