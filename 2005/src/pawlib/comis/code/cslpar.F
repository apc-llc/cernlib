*
* $Id: cslpar.F,v 1.1.1.1 1996/02/26 17:16:25 mclareni Exp $
*
* $Log: cslpar.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:25  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.00  by  Rene Brun
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSLPAR(N)
***---------------------
#include "comis/cswpar.inc"
      CALL CSXPAR(N)
      CSLPAR=NCHP
      END
