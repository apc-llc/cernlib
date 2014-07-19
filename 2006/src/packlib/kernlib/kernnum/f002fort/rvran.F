*
* $Id: rvran.F,v 1.1.1.1 1996/02/15 17:48:52 mclareni Exp $
*
* $Log: rvran.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:52  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RVRAN(N,A,B,Z,Z2)
          REAL                A, B, C, Z(*), Z2(*)
          IF(N .LE. 0)  RETURN
          LZJ  =  1
#include "rzj.inc"
          C    =  B - A
          DO 10     J  =  1, N
             Z(LZJ)  =  C*RANF() + A
             LZJ     =  LZJ + JZ
  10         CONTINUE
          RETURN
          END
