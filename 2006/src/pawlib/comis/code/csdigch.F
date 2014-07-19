*
* $Id: csdigch.F,v 1.1.1.1 1996/02/26 17:16:29 mclareni Exp $
*
* $Log: csdigch.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:29  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 21/02/94  12.29.58  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   09/12/93
      LOGICAL FUNCTION CSDIGCH(CH)
      CHARACTER *1 CH
#include "comis/csichv.inc"
      ICH=ICHAR(CH)
      CSDIGCH=.FALSE.
      IF( ICH .GE. ICHD0 .AND. ICH .LE. ICHD9 )CSDIGCH=.TRUE.
      END
