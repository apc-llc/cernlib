*
* $Id: rvmpa.F,v 1.1.1.1 1996/02/15 17:48:51 mclareni Exp $
*
* $Log: rvmpa.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:51  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          REAL FUNCTION       RVMPA(N,X,X2,Y,Y2,S)
          REAL                X(*), X2(*), Y(*), Y2(*), S, A, B
#if defined(CERNLIB_NUMRDBLE)
          DOUBLE PRECISION    SUM, MPA
          MPA(A,B,SUM)  =  DBLE(A)*DBLE(B) + SUM
#endif
#if !defined(CERNLIB_NUMRDBLE)
          REAL                SUM, MPA
          MPA(A,B,SUM)  =  A*B + SUM
#endif
          SUM  =  S
          IF(N .LE. 0)  GOTO 20
#include "rxj.inc"
#include "ryj.inc"
#include "vmpa.inc"
  20      RVMPA  =  SUM
          RETURN
          END
