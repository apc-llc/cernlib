*
* $Id: dvmpa.F,v 1.1.1.1 1996/02/15 17:48:51 mclareni Exp $
*
* $Log: dvmpa.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:51  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          DOUBLE PRECISION FUNCTION DVMPA(N,X,X2,Y,Y2,S)
          DOUBLE PRECISION    X(*), X2(*), Y(*), Y2(*), S, A, B
          DOUBLE PRECISION    SUM, MPA
          MPA(A,B,SUM)  =  A*B + SUM
          SUM  =  S
          IF(N .LE. 0)  GOTO 20
#include "dxj.inc"
#include "dyj.inc"
#include "vmpa.inc"
  20      DVMPA  =  SUM
          RETURN
          END
