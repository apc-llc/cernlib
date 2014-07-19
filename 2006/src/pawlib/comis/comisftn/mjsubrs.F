*
* $Id: mjsubrs.F,v 1.1.1.1 1996/02/26 17:16:49 mclareni Exp $
*
* $Log: mjsubrs.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:49  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_OLDVSUN)
*CMZ :  1.14/05 28/05/93  16.17.30  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      FUNCTION MJSUBR(I)
      MJSUBR=LOC(I)
      END
#endif
