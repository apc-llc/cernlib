*
* $Id: drscl.F,v 1.1.1.1 1996/02/15 17:48:59 mclareni Exp $
*
* $Log: drscl.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:59  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DRSCL(M,N,D,D2,X,X12,X21,Z,Z12,Z21)
       DOUBLE PRECISION D(*),D2(*),X(*),X12(*),X21(*),Z(*),Z12(*),Z21(*)
          IF(M .LE. 0  .OR.  N .LE. 0)  RETURN
#include "ddi.inc"
#include "dxij.inc"
#include "dzij.inc"
#include "rscl.inc"
