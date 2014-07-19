*
* $Id: rumns.F,v 1.1.1.1 1996/02/15 17:48:59 mclareni Exp $
*
* $Log: rumns.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:59  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RUMNS(N,U,U12,U22,Y,Y2,Z,Z2)
          REAL                U(*),U12(*),U22(*),Y(*),Y2(*),Z(*),Z2(*)
          REAL                A, B, SIGNF
#if defined(CERNLIB_NUMRDBLE)
          DOUBLE PRECISION    SUM, F
          F(A,B,SUM)  =  -DBLE(A)*DBLE(B) + SUM
#endif
#if !defined(CERNLIB_NUMRDBLE)
          REAL                SUM, F
          F(A,B,SUM)  =  -A*B + SUM
#endif
          SIGNF(A)  =  -A
          IF(N .LE. 0)  RETURN
#include "rujk.inc"
#include "ryj.inc"
#include "rzi.inc"
#include "umpa.inc"
