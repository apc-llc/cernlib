*
* $Id: nrdist.F,v 1.1.1.1 1996/02/15 17:48:46 mclareni Exp $
*
* $Log: nrdist.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:46  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          FUNCTION NRDIST(X,Y)
          REAL     X, Y
#include "kernnumt/sysdat.inc"
          RELERR  =  ABS(X-Y) / AMAX1(ABS(X),ABS(Y))
          NRDIST  =  NINT(RELERR/RELPR)
          RETURN
          END
