*
* $Id: gquad.F,v 1.1.1.1 1996/02/15 17:48:47 mclareni Exp $
*
* $Log: gquad.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:47  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
      FUNCTION GQUAD(F,A,B,N)
      EXTERNAL F
      DIMENSION X(1),W(1)
C
C     ******************************************************************
C
C     GQUAD IS SET EQUAL TO THE INTEGRAL OF THE FUNCTION F OVER THE IN-
C     TERVAL (A,B), AS APPROXIMATED BY THE GAUSS-LEGENDRE QUADRATURE
C     FORMULA OF ORDER N.
C
C     CALLS ... SUBROUTINE D106XX.
C
C     ******************************************************************
C
C  START.
      CALL D106XX(1,F,A,B,N,X,W)
      GQUAD=X(1)
      RETURN
      END
