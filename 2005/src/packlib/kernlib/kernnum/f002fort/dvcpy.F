*
* $Id: dvcpy.F,v 1.1.1.1 1996/02/15 17:48:51 mclareni Exp $
*
* $Log: dvcpy.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:51  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DVCPY(N,X,X2,Z,Z2)
          DOUBLE PRECISION    X(*), X2(*), Z(*), Z2(*), FUNCT, A
          FUNCT(A)  =  A
          IF(N .LE. 0)  RETURN
#include "dxj.inc"
#include "dzj.inc"
#include "vcpy.inc"
