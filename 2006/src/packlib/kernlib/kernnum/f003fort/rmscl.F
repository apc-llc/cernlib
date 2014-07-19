*
* $Id: rmscl.F,v 1.1.1.1 1996/02/15 17:48:58 mclareni Exp $
*
* $Log: rmscl.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:58  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RMSCL(M,N,S,X,X12,X21,Z,Z12,Z21)
          REAL                S, X(*),X12(*),X21(*), Z(*),Z12(*),Z21(*)
          REAL                FUNCT, A
          FUNCT(A)  =  S*A
          IF(M .LE. 0  .OR. N .LE. 0)  RETURN
#include "rxij.inc"
#include "rzij.inc"
#include "mcpy.inc"
