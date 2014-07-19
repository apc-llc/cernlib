*
* $Id: cvcpy.F,v 1.1.1.1 1996/02/15 17:48:51 mclareni Exp $
*
* $Log: cvcpy.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:51  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          CVCPY(N,X,X2,Z,Z2)
          COMPLEX             X(*), X2(*), Z(*), Z2(*), FUNCT, A
          FUNCT(A)  =  A
          IF(N .LE. 0)  RETURN
#include "cxj.inc"
#include "czj.inc"
#include "vcpy.inc"
