*
* $Id: tamuna.F,v 1.1.1.1 1996/02/15 17:48:44 mclareni Exp $
*
* $Log: tamuna.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:44  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE          RAMUNA(IDIM,X,Y,Z)
          REAL                X(*), Y(*), Z(*)
#include "amuna.inc"
          SUBROUTINE          DAMUNA(IDIM,X,Y,Z)
          DOUBLE PRECISION    X(*), Y(*), Z(*)
#include "amuna.inc"
          SUBROUTINE          CAMUNA(IDIM,X,Y,Z)
          COMPLEX             X(*), Y(*), Z(*)
#include "amuna.inc"
