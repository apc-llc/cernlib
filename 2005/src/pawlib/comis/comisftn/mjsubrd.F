*
* $Id: mjsubrd.F,v 1.1.1.1 1996/02/26 17:16:49 mclareni Exp $
*
* $Log: mjsubrd.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:49  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_UNIX)
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MJSUBR(I)
      MJSUBR=LOCB(I)
      END
#endif
