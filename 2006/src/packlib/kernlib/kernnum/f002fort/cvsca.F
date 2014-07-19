*
* $Id: cvsca.F,v 1.1.1.1 1996/02/15 17:48:52 mclareni Exp $
*
* $Log: cvsca.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:52  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          CVSCA(N,S,X,X2,Y,Y2,Z,Z2)
          COMPLEX             S, X(*), X2(*), Y(*), Y2(*), Z(*), Z2(*)
          COMPLEX             ADD, A, B
          ADD(A,B)  =  S*A + B
          IF(N .LE. 0)  RETURN
#include "cxj.inc"
#include "cyj.inc"
#include "czj.inc"
#include "vadd.inc"
