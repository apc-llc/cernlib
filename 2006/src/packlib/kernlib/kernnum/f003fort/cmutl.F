*
* $Id: cmutl.F,v 1.1.1.1 1996/02/15 17:48:59 mclareni Exp $
*
* $Log: cmutl.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:59  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          CMUTL(N,X,X12,X21)
          COMPLEX             X(*), X12(*), X21(*)
          IF(N .LE. 1)  RETURN
#include "cxij.inc"
#include "mutl.inc"
