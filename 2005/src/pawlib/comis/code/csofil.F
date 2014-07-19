*
* $Id: csofil.F,v 1.1.1.1 1996/02/26 17:16:25 mclareni Exp $
*
* $Log: csofil.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:25  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.00  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSOFIL(LUN,NAME)
***---------------------------------
#include "comis/cslun.inc"
         CHARACTER *(*) NAME
      DO 1 K=1,4
      IF(LUNARR(K).EQ.LUN)THEN
        IF(K.NE.2)THEN
          CALL CSSOUT('CSOFIL: CHANNEL IS RESERVED')
          RETURN
        ENDIF
      ENDIF
  1   CONTINUE
      LUNFIL=LUN
      CALL CSFILE(NAME)
      END
