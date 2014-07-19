*
* $Id: dzdgfl.F,v 1.1.1.1 1996/03/04 16:13:09 mclareni Exp $
*
* $Log: dzdgfl.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:09  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDGFL(CLINE,IST,IEND)
      CHARACTER*(*) CLINE
      IST=0
      IEND=0
*     point after (
      IFC=INDEX(CLINE(1:),'(')
      IF(IFC.LE.0)GOTO 999
      IFC=IFC+1
      ILC=INDEX(CLINE(IFC:),':')
      IF(ILC.LE.0)GOTO 999
      ILC=ILC+IFC-2
      CALL DZDCTI(CLINE(IFC:ILC),IST)
      IF(IST.LE.0)GOTO 999
*     point after :
      IFC=ILC+2
      ILC=INDXNC(CLINE(IFC:))
      IF(ILC.LE.0)THEN
         ILC=LNBLNK(CLINE)
      ELSE
         ILC=ILC+IFC-2
      ENDIF
      CALL DZDCTI(CLINE(IFC:ILC),IEND)
999   CONTINUE
      END
************************************************************************
