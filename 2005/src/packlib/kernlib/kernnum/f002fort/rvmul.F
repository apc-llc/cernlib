*
* $Id: rvmul.F,v 1.1.1.1 1996/02/15 17:48:52 mclareni Exp $
*
* $Log: rvmul.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:52  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE RVMUL(N,X,X2,Y,Y2,Z,Z2)
          REAL                X(*), X2(*), Y(*), Y2(*), Z(*), Z2(*)
          REAL                ADD, A, B
          ADD(A,B)  =  A*B
          IF(N .LE. 0)  RETURN
#include "rxj.inc"
#include "ryj.inc"
#include "rzj.inc"
#include "vadd.inc"
