*
* $Id: dvran.F,v 1.1.1.1 1996/02/15 17:48:52 mclareni Exp $
*
* $Log: dvran.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:52  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DVRAN(N,A,B,Z,Z2)
          DOUBLE PRECISION    A, B, C, Z(*), Z2(*)
          DOUBLE PRECISION    DRANF
          IF(N .LE. 0)  RETURN
          LZJ  =  1
#include "dzj.inc"
          C    =  B - A
          DO 10     J  =  1, N
             Z(LZJ)  =  C*DRANF() + A
             LZJ     =  LZJ + JZ
  10         CONTINUE
          RETURN
          END
