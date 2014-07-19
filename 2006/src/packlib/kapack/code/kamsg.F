*
* $Id: kamsg.F,v 1.1.1.1 1996/03/08 11:40:51 mclareni Exp $
*
* $Log: kamsg.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:51  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAMSG(LUN,STRING,IRC)
*
*.....WRITE A USER MESSAGE IN THE TRANSACTION LOG
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
*
      CHARACTER STRING*(*)
*
*-----------------------------------------------------------------------
*
      CALL KAXINN(LUN)
*
      IF ( LUNLOG .NE. 0 )  THEN
         CALL KAXMSG(LUNLOG,STRING)
         IRC = 0
      ELSE
         IRC = 1
      ENDIF
*
      END
