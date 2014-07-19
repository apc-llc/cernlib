*
* $Id: csknch.F,v 1.1.1.1 1996/02/26 17:16:18 mclareni Exp $
*
* $Log: csknch.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:18  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.36  by  Rene Brun
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSKNCH(J,N)
***------------------------------
*      gets next non blank character
***--------------------------------
  1   CSKNCH=MKBLAN(J,N)
      IF(N.EQ.0)THEN
        CALL CSRD(J,N)
        IF(N.LT.0)RETURN
        GO TO 1
      ENDIF
      J=J+1
      N=N-1
      END
