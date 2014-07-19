*
* $Id: dmmna.F,v 1.1.1.1 1996/02/15 17:48:57 mclareni Exp $
*
* $Log: dmmna.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:57  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DMMNA(M,N,X,X12,X21,Y,Y2,Z,Z2)
          DOUBLE PRECISION    X(*),X12(*),X21(*),Y(*),Y2(*),Z(*),Z2(*)
          DOUBLE PRECISION    A, B, SUM, F, SIGNF
          F(A,B,SUM)  =  -A*B + SUM
          SIGNF(A)    =  A
          IF(M .LE. 0  .OR.  N .LE. 0)  RETURN
#include "dxij.inc"
#include "dyj.inc"
#include "dzi.inc"
#include "mmpa.inc"
