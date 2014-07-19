*
* $Id: dvset.F,v 1.1.1.1 1996/02/15 17:48:53 mclareni Exp $
*
* $Log: dvset.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:53  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DVSET(N,S,Z,Z2)
          DOUBLE PRECISION    S, Z(*), Z2(*)
          IF(N .LE. 0)  RETURN
#include "dzj.inc"
#include "vset.inc"
