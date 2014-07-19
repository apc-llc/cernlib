*
* $Id: cmadd.F,v 1.1.1.1 1996/02/15 17:48:56 mclareni Exp $
*
* $Log: cmadd.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          CMADD(M,N,X,X12,X21,Y,Y12,Y21,Z,Z12,Z21)
          COMPLEX             X(*), X12(*), X21(*), Y(*), Y12(*), Y21(*)
          COMPLEX             Z(*), Z12(*), Z21(*), ADD,  A,      B
          ADD(A,B)  =  A+B
          IF(M .LE. 0  .OR.  N .LE. 0)  RETURN
#include "cxij.inc"
#include "cyij.inc"
#include "czij.inc"
#include "madd.inc"
