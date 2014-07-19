*
* $Id: dvsub.F,v 1.1.1.1 1996/02/15 17:48:53 mclareni Exp $
*
* $Log: dvsub.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:53  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DVSUB(N,X,X2,Y,Y2,Z,Z2)
          DOUBLE PRECISION    X(*), X2(*), Y(*), Y2(*), Z(*), Z2(*)
          DOUBLE PRECISION    ADD, A, B
          ADD(A,B)  =  A-B
          IF(N .LE. 0)  RETURN
#include "dxj.inc"
#include "dyj.inc"
#include "dzj.inc"
#include "vadd.inc"
