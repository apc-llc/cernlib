*
* $Id: fm0tob.F,v 1.1.1.1 1996/03/07 15:18:20 mclareni Exp $
*
* $Log: fm0tob.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:20  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FM0TOB(STRING)
*
*     Convert zero bytes in a character string to character blanks
*
*
      CHARACTER*(*) STRING
 
      DO 10 J=1,LEN(STRING)
         IF ( ICHAR(STRING(J:J)) .EQ. 0 ) STRING(J:J)=' '
   10 CONTINUE
      END
