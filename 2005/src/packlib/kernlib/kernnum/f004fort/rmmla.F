*
* $Id: rmmla.F,v 1.1.1.1 1996/02/15 17:49:01 mclareni Exp $
*
* $Log: rmmla.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:01  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RMMLA(M,N,K,X,X12,X21,Y,Y12,Y21,Z,Z12,Z21)
          REAL                X(*), X12(*), X21(*), Y(*), Y12(*), Y21(*)
          REAL                Z(*), Z12(*), Z21(*)
#include "rdotf.inc"
#include "rlocf.inc"
#include "mla.inc"
          END
