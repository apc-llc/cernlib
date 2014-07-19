*
* $Id: cmnms.F,v 1.1.1.1 1996/02/15 17:49:02 mclareni Exp $
*
* $Log: cmnms.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:02  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          CMNMS(M,N,K,X,X12,X21,Y,Y12,Y21,Z,Z12,Z21)
          COMPLEX             X(*), X12(*), X21(*), Y(*), Y12(*), Y21(*)
          COMPLEX             Z(*), Z12(*), Z21(*)
#include "cdotf.inc"
#include "dlocf.inc"
#include "nms.inc"
          END
