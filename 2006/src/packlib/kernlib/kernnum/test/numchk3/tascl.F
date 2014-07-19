*
* $Id: tascl.F,v 1.1.1.1 1996/02/15 17:48:44 mclareni Exp $
*
* $Log: tascl.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:44  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE          RASCL(IDIM,S,X,Z)
          REAL                S, X(*), Z(*)
#include "ascl.inc"
          SUBROUTINE          DASCL(IDIM,S,X,Z)
          DOUBLE PRECISION    S, X(*), Z(*)
#include "ascl.inc"
          SUBROUTINE          CASCL(IDIM,S,X,Z)
          COMPLEX             S, X(*), Z(*)
#include "ascl.inc"
