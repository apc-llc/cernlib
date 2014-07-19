*
* $Id: dmmlt.F,v 1.1.1.1 1996/02/15 17:49:01 mclareni Exp $
*
* $Log: dmmlt.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:01  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE        DMMLT(M,N,K,X,X12,X21,Y,Y12,Y21,Z,Z12,Z21,T)
          DOUBLE PRECISION    X(*), X12(*), X21(*), Y(*), Y12(*), Y21(*)
          DOUBLE PRECISION    Z(*), Z12(*), Z21(*), T(*), A, B, CNJF
          DOUBLE PRECISION    ZERO, SUM, DOTF, SQRF
          DOUBLE PRECISION    S11, S21, S22, S31, S41, S51, S52
          DATA      ZERO      /  0.D0  /
          DOTF(A,B,SUM)  =  A*B + SUM
          SQRF(A,SUM)    =  A*A + SUM
          CNJF(A)        =  A
#include "dlocf.inc"
#include "mmlt.inc"
          END
