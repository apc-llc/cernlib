*
* $Id: rmbil.F,v 1.1.1.1 1996/02/15 17:48:56 mclareni Exp $
*
* $Log: rmbil.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          REAL FUNCTION RMBIL(N,X,X2,Y,Y12,Y21,Z,Z2)
          REAL X(*),X2(*),Y(*),Y12(*),Y21(*),Z(*),Z2(*)
          REAL                A, B
#if !defined(CERNLIB_NUMRDBLE)
          REAL            SUM, ZERO, F, G, SXYZ, SYZ
          F(A,B,SUM)    =  A*B + SUM
          G(SYZ,B,SUM)  =  SYZ*B + SUM
          DATA      ZERO      /  0.  /
#endif
#if defined(CERNLIB_NUMRDBLE)
          DOUBLE PRECISION    SUM, ZERO, F, G, SXYZ, SYZ
          F(A,B,SUM)    =  DBLE(A)*DBLE(B) + SUM
          G(SYZ,B,SUM)  =  SYZ*DBLE(B) + SUM
          DATA      ZERO      /  0.D0 /
#endif
          SXYZ  =  ZERO
          IF(N .LE. 0)  GOTO 20
#include "rxi.inc"
#include "ryij.inc"
#include "rzj.inc"
#include "mbil.inc"
  20      RMBIL  =  SXYZ
          RETURN
          END
