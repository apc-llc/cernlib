*
* $Id: cskide.F,v 1.1.1.1 1996/02/26 17:16:18 mclareni Exp $
*
* $Log: cskide.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:18  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.36  by  Rene Brun
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSKIDE(J,L)
***--------------------------
*    returns type using first letter only
***-----------------------
         CHARACTER FL
      JS=MJSCHA(FL)
      CALL CCOPYS(J,JS,1)
      CSKIDE=-1
      L=1
      IF(FL.LT.'I'.OR.FL.GT.'N')CSKIDE=-2
      END
