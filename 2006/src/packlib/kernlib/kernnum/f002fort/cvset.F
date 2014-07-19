*
* $Id: cvset.F,v 1.1.1.1 1996/02/15 17:48:53 mclareni Exp $
*
* $Log: cvset.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:53  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          CVSET(N,S,Z,Z2)
          COMPLEX             S, Z(*), Z2(*)
          IF(N .LE. 0)  RETURN
#include "czj.inc"
#include "vset.inc"
