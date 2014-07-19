*
* $Id: dmnms.F,v 1.1.1.1 1996/02/15 17:49:02 mclareni Exp $
*
* $Log: dmnms.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:02  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DMNMS(M,N,K,X,X12,X21,Y,Y12,Y21,Z,Z12,Z21)
          DOUBLE PRECISION    X(*), X12(*), X21(*), Y(*), Y12(*), Y21(*)
          DOUBLE PRECISION    Z(*), Z12(*), Z21(*)
#include "ddotf.inc"
#include "dlocf.inc"
#include "nms.inc"
          END
