*
* $Id: mjchard.F,v 1.2 1996/03/14 13:43:51 berezhno Exp $
*
* $Log: mjchard.F,v $
* Revision 1.2  1996/03/14 13:43:51  berezhno
* mods for WINNT
*
* Revision 1.1.1.1  1996/02/26 17:16:48  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_UNIX) && !defined(CERNLIB_WINNT) ||defined(CERNLIB_IBM)
*CMZ :  1.18/00 04/03/94  16.14.29  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      FUNCTION MJCHAR(I)
***--------------------
*   gets byte-address
***--------------------
      MJCHAR=LOCB(I)
      END
#endif
