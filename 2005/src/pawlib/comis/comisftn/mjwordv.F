*
* $Id: mjwordv.F,v 1.1.1.1 1996/02/26 17:16:50 mclareni Exp $
*
* $Log: mjwordv.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:50  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_VAX)
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MJWORD(I)
      MJWORD=%LOC(I)/4
      END
#endif
