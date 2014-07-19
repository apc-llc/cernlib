*
* $Id: mikcha.F,v 1.1.1.1 1996/02/26 17:16:42 mclareni Exp $
*
* $Log: mikcha.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:42  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.05  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MIKCHA(J)
      CHARACTER*4 CH
      CALL CCOPYS(J,MJSCHA(CH),1)
      MIKCHA=ICHAR(CH(1:1))
      END
