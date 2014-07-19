*
* $Id: dvmuna.F,v 1.1.1.1 1996/02/15 17:48:52 mclareni Exp $
*
* $Log: dvmuna.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:52  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DVMUNA(N,X,X2,Y,Y2,Z,Z2)
          DOUBLE PRECISION    X(*), X2(*), Y(*), Y2(*), Z(*), Z2(*)
          DOUBLE PRECISION    MULA, A, B, C
          MULA(A,B,C)  =  -A*B + C
          IF(N .LE. 0)  RETURN
#include "dxj.inc"
#include "dyj.inc"
#include "dzj.inc"
#include "vmula.inc"
