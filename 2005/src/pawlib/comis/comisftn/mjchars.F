*
* $Id: mjchars.F,v 1.1.1.1 1996/02/26 17:16:48 mclareni Exp $
*
* $Log: mjchars.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:48  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_OLDVSUN)||defined(CERNLIB_WINNT)
*CMZ :          10/11/95  14.29.37  by  Julian Bunn
*-- Author : V.Berezhnoi
      FUNCTION MJCHAR(I)
***--------------------
*   gets byte-address
***--------------------
      MJCHAR=LOC(I)
      END
#endif
