*
* $Id: dmbil.F,v 1.1.1.1 1996/02/15 17:48:56 mclareni Exp $
*
* $Log: dmbil.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          DOUBLE PRECISION FUNCTION DMBIL(N,X,X2,Y,Y12,Y21,Z,Z2)
          DOUBLE PRECISION X(*),X2(*),Y(*),Y12(*),Y21(*),Z(*),Z2(*)
          DOUBLE PRECISION A, B, SUM, ZERO, F, G, SXYZ, SYZ
          F(A,B,SUM)  =  A*B + SUM
          G(A,B,SUM)  =  A*B + SUM
          DATA      ZERO      /  0.D0  /
          SXYZ  =  ZERO
          IF(N .LE. 0)  GOTO 20
#include "dxi.inc"
#include "dyij.inc"
#include "dzj.inc"
#include "mbil.inc"
  20      DMBIL  =  SXYZ
          RETURN
          END
