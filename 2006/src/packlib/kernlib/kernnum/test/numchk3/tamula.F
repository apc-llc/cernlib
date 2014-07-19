*
* $Id: tamula.F,v 1.1.1.1 1996/02/15 17:48:44 mclareni Exp $
*
* $Log: tamula.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:44  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE          RAMULA(IDIM,X,Y,Z)
          REAL                X(*), Y(*), Z(*)
#include "amula.inc"
          SUBROUTINE          DAMULA(IDIM,X,Y,Z)
          DOUBLE PRECISION    X(*), Y(*), Z(*)
#include "amula.inc"
          SUBROUTINE          CAMULA(IDIM,X,Y,Z)
          COMPLEX             X(*), Y(*), Z(*)
#include "amula.inc"
