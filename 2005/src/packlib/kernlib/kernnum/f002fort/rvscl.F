*
* $Id: rvscl.F,v 1.1.1.1 1996/02/15 17:48:52 mclareni Exp $
*
* $Log: rvscl.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:52  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RVSCL(N,S,X,X2,Z,Z2)
          REAL                S, X(*), X2(*), Z(*), Z2(*), FUNCT, A
          FUNCT(A)  =  S*A
          IF(N .LE. 0)  RETURN
#include "rxj.inc"
#include "rzj.inc"
#include "vcpy.inc"
