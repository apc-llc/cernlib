*
* $Id: mjchara.F,v 1.1.1.1 1996/02/26 17:16:48 mclareni Exp $
*
* $Log: mjchara.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:48  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_APOLLO)
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MJCHAR(I)
***--------------------
*   gets byte-address
***--------------------
      MJCHAR=IADDR(I)
      END
#endif
