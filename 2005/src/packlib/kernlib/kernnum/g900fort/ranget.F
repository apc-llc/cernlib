*
* $Id: ranget.F,v 1.1.1.1 1996/02/15 17:49:05 mclareni Exp $
*
* $Log: ranget.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:05  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE RANGET(SEED)
          DOUBLE PRECISION    SEED,     G900GT,   G900ST,   DUMMY
          SEED  =  G900GT()
          RETURN
          ENTRY RANSET(SEED)
          DUMMY  =  G900ST(SEED)
          RETURN
          END
