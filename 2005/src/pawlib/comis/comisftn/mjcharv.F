*
* $Id: mjcharv.F,v 1.1.1.1 1996/02/26 17:16:48 mclareni Exp $
*
* $Log: mjcharv.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:48  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_VAX)
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MJCHAR(I)
      MJCHAR=%LOC(I)
      END
#endif
