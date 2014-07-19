*
* $Id: alphch.F,v 1.1.1.1 1996/02/26 17:16:31 mclareni Exp $
*
* $Log: alphch.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:31  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 10/02/94  16.21.20  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   09/12/93
      LOGICAL FUNCTION ALPHCH(CH)
      CHARACTER *1 CH
      ALPHCH=.FALSE.
      IF( CH .GE. 'A' .AND. CH .LE. 'Z'
     + .OR.
     +    CH .GE. 'a' .AND. CH .LE. 'z'
     + .OR.
     +    CH .EQ. '_'                 ) ALPHCH=.TRUE.
      END
