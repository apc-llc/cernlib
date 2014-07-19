*
* $Id: rdigi.F,v 1.1.1.1 1996/02/15 17:47:46 mclareni Exp $
*
* $Log: rdigi.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:46  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      LOGICAL FUNCTION RDIGI()
C** TO FIND TYPE OF A CHARACTER 'CHAR'
C*******************************************************************
#include "i303cd.inc"
      CHARACTER XXXXXX*10
C*NS  CHARACTER*1 CHAR
      DATA XXXXXX/'0123456789'/
      RDIGI=.FALSE.
      IF (INDEX(XXXXXX,CURNT) .GT. 0) RDIGI=.TRUE.
      RETURN
      END
