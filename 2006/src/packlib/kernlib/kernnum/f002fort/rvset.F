*
* $Id: rvset.F,v 1.1.1.1 1996/02/15 17:48:53 mclareni Exp $
*
* $Log: rvset.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:53  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RVSET(N,S,Z,Z2)
          REAL                S, Z(*), Z2(*)
          IF(N .LE. 0)  RETURN
#include "rzj.inc"
#include "vset.inc"
