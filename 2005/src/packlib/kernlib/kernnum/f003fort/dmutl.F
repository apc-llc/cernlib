*
* $Id: dmutl.F,v 1.1.1.1 1996/02/15 17:48:59 mclareni Exp $
*
* $Log: dmutl.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:59  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DMUTL(N,X,X12,X21)
          DOUBLE PRECISION    X(*), X12(*), X21(*)
          IF(N .LE. 1)  RETURN
#include "dxij.inc"
#include "mutl.inc"
