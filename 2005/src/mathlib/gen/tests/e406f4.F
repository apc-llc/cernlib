*
* $Id: e406f4.F,v 1.1.1.1 1996/04/01 15:01:26 mclareni Exp $
*
* $Log: e406f4.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:26  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      FUNCTION E406F4(X)
C     Here we specify an odd function
#include "gen/def64.inc"
     +   E406F4,X
      E406F4=X*X*SIN(X)
      RETURN
      END
