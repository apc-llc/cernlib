*
* $Id: rmmpy.F,v 1.1.1.1 1996/02/15 17:48:57 mclareni Exp $
*
* $Log: rmmpy.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:57  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RMMPY(M,N,X,X12,X21,Y,Y2,Z,Z2)
          REAL                X(*),X12(*),X21(*),Y(*),Y2(*),Z(*),Z2(*)
#if defined(CERNLIB_NUMRDBLE)
          REAL                A, B
          DOUBLE PRECISION    SUM, ZERO, F
          F(A,B,SUM)  =  DBLE(A)*DBLE(B) + SUM
          DATA ZERO    / 0.D0 /
#endif
#if !defined(CERNLIB_NUMRDBLE)
          REAL                A, B, SUM, ZERO, F
          F(A,B,SUM)  =  A*B + SUM
          DATA ZERO    / 0. /
#endif
          IF(M .LE. 0  .OR.  N .LE. 0)  RETURN
#include "rxij.inc"
#include "ryj.inc"
#include "rzi.inc"
#include "mmpy.inc"
