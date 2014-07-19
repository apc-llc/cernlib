*
* $Id: rzero.F,v 1.1.1.1 1996/02/15 17:49:07 mclareni Exp $
*
* $Log: rzero.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:07  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
      SUBROUTINE RZERO(A,B,X0,R,EPS,MXF,F)
      IMPLICIT REAL (A-H,O-Z)
#include "c205body.inc"
  100 FORMAT(1X,'***** CERN C205 RZERO ... F(A) AND F(B)',
     1          ' HAVE THE SAME SIGN')
  101 FORMAT(1X,'***** CERN C205 RZERO ... TOO MANY FUNCTION CALLS')
      END
