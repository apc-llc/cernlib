*
* $Id: dvsca.F,v 1.1.1.1 1996/02/15 17:48:52 mclareni Exp $
*
* $Log: dvsca.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:52  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DVSCA(N,S,X,X2,Y,Y2,Z,Z2)
          DOUBLE PRECISION    S, X(*), X2(*), Y(*), Y2(*), Z(*), Z2(*)
          DOUBLE PRECISION    ADD, A, B
          ADD(A,B)  =  S*A + B
          IF(N .LE. 0)  RETURN
#include "dxj.inc"
#include "dyj.inc"
#include "dzj.inc"
#include "vadd.inc"
