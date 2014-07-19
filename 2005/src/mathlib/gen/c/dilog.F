*
* $Id: dilog.F,v 1.1.1.1 1996/04/01 15:02:04 mclareni Exp $
*
* $Log: dilog.F,v $
* Revision 1.1.1.1  1996/04/01 15:02:04  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
#if !defined(CERNLIB_DOUBLE)
      FUNCTION DILOG(X)
      DILOG = RDILOG(X)
      END
#endif
