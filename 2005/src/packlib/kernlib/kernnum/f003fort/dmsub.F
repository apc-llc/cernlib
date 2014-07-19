*
* $Id: dmsub.F,v 1.1.1.1 1996/02/15 17:48:59 mclareni Exp $
*
* $Log: dmsub.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:59  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DMSUB(M,N,X,X12,X21,Y,Y12,Y21,Z,Z12,Z21)
          DOUBLE PRECISION    X(*), X12(*), X21(*), Y(*), Y12(*), Y21(*)
          DOUBLE PRECISION    Z(*), Z12(*), Z21(*), ADD,  A,      B
          ADD(A,B)  =  A-B
#include "dxij.inc"
#include "dyij.inc"
#include "dzij.inc"
#include "madd.inc"
