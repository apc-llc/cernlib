*
* $Id: mjworda.F,v 1.1.1.1 1996/02/26 17:16:50 mclareni Exp $
*
* $Log: mjworda.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:50  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_IBM)||defined(CERNLIB_APOLLO)||defined(CERNLIB_UNIX)
*CMZ :  1.12/05 20/03/92  16.04.19  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MJWORD(I)
      MJWORD=LOCF(I)
      END
#endif
