*
* $Id: dumpy.F,v 1.1.1.1 1996/02/15 17:49:00 mclareni Exp $
*
* $Log: dumpy.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:00  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DUMPY(N,U,U12,U22,Y,Y2,Z,Z2)
          DOUBLE PRECISION    U(*),U12(*),U22(*),Y(*),Y2(*),Z(*),Z2(*)
          DOUBLE PRECISION    A, B, SUM, ZERO, F
          F(A,B,SUM)  =  A*B + SUM
          DATA ZERO    / 0.D0 /
          IF(N .LE. 0)  RETURN
#include "dujk.inc"
#include "dyj.inc"
#include "dzi.inc"
#include "umpy.inc"
