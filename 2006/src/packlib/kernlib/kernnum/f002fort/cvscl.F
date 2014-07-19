*
* $Id: cvscl.F,v 1.1.1.1 1996/02/15 17:48:52 mclareni Exp $
*
* $Log: cvscl.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:52  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          CVSCL(N,S,X,X2,Z,Z2)
          COMPLEX             S, X(*), X2(*), Z(*), Z2(*), FUNCT, A
          FUNCT(A)  =  S*A
          IF(N .LE. 0)  RETURN
#include "cxj.inc"
#include "czj.inc"
#include "vcpy.inc"
