*
* $Id: dvdiv.F,v 1.1.1.1 1996/02/15 17:48:51 mclareni Exp $
*
* $Log: dvdiv.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:51  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DVDIV(N,X,X2,Y,Y2,Z,Z2,IFAIL)
          DOUBLE PRECISION    X(*), X2(*), Y(*), Y2(*), Z(*), Z2(*), T
          REALF(T)  =  SNGL(T)
          IFAIL     =  0
          IF(N .LE. 0)  RETURN
#include "dxj.inc"
#include "dyj.inc"
#include "dzj.inc"
#include "vdiv.inc"
