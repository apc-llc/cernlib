*
* $Id: mjsubrv.F,v 1.1.1.1 1996/02/26 17:16:50 mclareni Exp $
*
* $Log: mjsubrv.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:50  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_VAX)
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MJSUBR(I)
      EXTERNAL I
      MJSUBR=JUMPAD(I)
      END
#endif
