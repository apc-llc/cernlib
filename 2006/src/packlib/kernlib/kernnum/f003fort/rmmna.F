*
* $Id: rmmna.F,v 1.1.1.1 1996/02/15 17:48:57 mclareni Exp $
*
* $Log: rmmna.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:57  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RMMNA(M,N,X,X12,X21,Y,Y2,Z,Z2)
          REAL                X(*),X12(*),X21(*),Y(*),Y2(*),Z(*),Z2(*)
          REAL                A, B, SIGNF
#if defined(CERNLIB_NUMRDBLE)
          DOUBLE PRECISION    SUM, F
          F(A,B,SUM)  =  -DBLE(A)*DBLE(B) + SUM
#endif
#if !defined(CERNLIB_NUMRDBLE)
          REAL                SUM, F
          F(A,B,SUM)  =  -A*B + SUM
#endif
          SIGNF(A)    =  A
          IF(M .LE. 0  .OR.  N .LE. 0)  RETURN
#include "rxij.inc"
#include "ryj.inc"
#include "rzi.inc"
#include "mmpa.inc"
