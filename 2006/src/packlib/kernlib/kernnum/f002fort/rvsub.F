*
* $Id: rvsub.F,v 1.1.1.1 1996/02/15 17:48:53 mclareni Exp $
*
* $Log: rvsub.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:53  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE RVSUB(N,X,X2,Y,Y2,Z,Z2)
          REAL                X(*), X2(*), Y(*), Y2(*), Z(*), Z2(*)
          REAL                ADD, A, B
          ADD(A,B)  =  A-B
          IF(N .LE. 0)  RETURN
#include "rxj.inc"
#include "ryj.inc"
#include "rzj.inc"
#include "vadd.inc"
