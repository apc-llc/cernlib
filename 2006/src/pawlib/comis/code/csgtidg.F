*
* $Id: csgtidg.F,v 1.1.1.1 1996/02/26 17:16:29 mclareni Exp $
*
* $Log: csgtidg.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:29  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 30/09/93  17.25.51  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSGTIDG(IND,NAME,NC)
***--------------------------------------
      CHARACTER *(*)NAME
#include "comis/cstabps.inc"
      CALL CSGTID(IND,KSIDG,NAME,NC)
      END
