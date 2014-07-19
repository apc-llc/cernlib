*
* $Id: csolog.F,v 1.1.1.1 1996/02/26 17:16:25 mclareni Exp $
*
* $Log: csolog.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:25  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.00  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSOLOG(LUN,NAME)
***---------------------------------
#include "comis/cslun.inc"
         CHARACTER *(*) NAME
      DO 1 K=1,4
      IF(LUNARR(K).EQ.LUN)THEN
        IF(K.NE.3)THEN
          CALL CSSOUT('CSOLOG: CHANNEL IS RESERVED')
          RETURN
        ENDIF
      ENDIF
  1   CONTINUE
      LUNLOG=LUN
      CALL CSLOG(NAME)
      END
