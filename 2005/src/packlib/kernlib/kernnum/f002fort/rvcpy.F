*
* $Id: rvcpy.F,v 1.1.1.1 1996/02/15 17:48:51 mclareni Exp $
*
* $Log: rvcpy.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:51  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RVCPY(N,X,X2,Z,Z2)
          REAL                X(*), X2(*), Z(*), Z2(*), FUNCT, A
          FUNCT(A)  =  A
          IF(N .LE. 0)  RETURN
#include "rxj.inc"
#include "rzj.inc"
#include "vcpy.inc"
