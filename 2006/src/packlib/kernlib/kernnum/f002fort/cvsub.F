*
* $Id: cvsub.F,v 1.1.1.1 1996/02/15 17:48:53 mclareni Exp $
*
* $Log: cvsub.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:53  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          CVSUB(N,X,X2,Y,Y2,Z,Z2)
          COMPLEX             X(*), X2(*), Y(*), Y2(*), Z(*), Z2(*)
          COMPLEX             ADD, A, B
          ADD(A,B)  =  A-B
          IF(N .LE. 0)  RETURN
#include "cxj.inc"
#include "cyj.inc"
#include "czj.inc"
#include "vadd.inc"
