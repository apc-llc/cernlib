*
* $Id: csanumch.F,v 1.1.1.1 1996/02/26 17:16:31 mclareni Exp $
*
* $Log: csanumch.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:31  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :          14/04/95  15.18.26  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   09/12/93
      LOGICAL FUNCTION CSANUMCH(CH)
      CHARACTER *1 CH
#include "comis/csichv.inc"
      ICH=ICHAR(CH)
      CSANUMCH=.FALSE.
      IF( ICH .GE. ICHLA .AND. ICH .LE. ICHLZ
     + .OR.
     +    CH .GE. 'a' .AND. CH .LE. 'z'
     + .OR.
     +    ICH .EQ. ICHSUSC
     + .OR.
     +    ICH .GE. ICHD0 .AND. ICH .LE. ICHD9
     +                                 ) CSANUMCH=.TRUE.
      END
