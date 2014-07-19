*
* $Id: rvmula.F,v 1.1.1.1 1996/02/15 17:48:52 mclareni Exp $
*
* $Log: rvmula.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:52  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE RVMULA(N,X,X2,Y,Y2,Z,Z2)
          REAL                X(*), X2(*), Y(*), Y2(*), Z(*), Z2(*)
          REAL                MULA, A, B, C
          MULA(A,B,C)  =  A*B + C
          IF(N .LE. 0)  RETURN
#include "rxj.inc"
#include "ryj.inc"
#include "rzj.inc"
#include "vmula.inc"
