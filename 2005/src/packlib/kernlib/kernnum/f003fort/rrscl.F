*
* $Id: rrscl.F,v 1.1.1.1 1996/02/15 17:48:59 mclareni Exp $
*
* $Log: rrscl.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:59  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RRSCL(M,N,D,D2,X,X12,X21,Z,Z12,Z21)
          REAL          D(*),D2(*),X(*),X12(*),X21(*),Z(*),Z12(*),Z21(*)
          IF(M .LE. 0  .OR.  N .LE. 0)  RETURN
#include "rdi.inc"
#include "rxij.inc"
#include "rzij.inc"
#include "rscl.inc"
