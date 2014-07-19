*
* $Id: dmcpy.F,v 1.1.1.1 1996/02/15 17:48:56 mclareni Exp $
*
* $Log: dmcpy.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DMCPY(M,N,X,X12,X21,Z,Z12,Z21)
          DOUBLE PRECISION    X(*),X12(*),X21(*),Z(*),Z12(*),Z21(*)
          DOUBLE PRECISION    FUNCT, A
          FUNCT(A)  =  A
          IF(M .LE. 0  .OR.  N .LE. 0)  RETURN
#include "dxij.inc"
#include "dzij.inc"
#include "mcpy.inc"
