*
* $Id: csomap.F,v 1.1.1.1 1996/02/26 17:16:25 mclareni Exp $
*
* $Log: csomap.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:25  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.59  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSOMAP(LUN,NAME)
             CHARACTER *(*) NAME
#include "comis/cslun.inc"
      DO 1 K=1,4
      IF(LUNARR(K).EQ.LUN)THEN
        IF(K.NE.4)THEN
          CALL CSSOUT('CSOMAP: CHANNEL IS RESERVED')
          RETURN
        ENDIF
      ENDIF
  1   CONTINUE
      LUNMAP=LUN
      CALL CSMAP(NAME)
      END
