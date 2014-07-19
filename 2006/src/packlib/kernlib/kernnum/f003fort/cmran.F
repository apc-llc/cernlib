*
* $Id: cmran.F,v 1.1.1.1 1996/02/15 17:48:58 mclareni Exp $
*
* $Log: cmran.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:58  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          CMRAN(M,N,A,B,Z,Z12,Z21)
          COMPLEX             A, B, Z(*), Z12(*), Z21(*), C
          IF(M .LE. 0  .OR. N .LE. 0)  RETURN
#include "czij.inc"
          MM  =  M
          NN  =  N
          IF(MM .GT. NN)  THEN
             MN  =  NN
             NN  =  MM
             MM  =  MN
             IJ  =  JZ
             JZ  =  IZ
             IZ  =  IJ
          ENDIF
          C     =  B - A
          LZI1  =  1
          DO 12     I  =  1, MM
             LZIJ  =  LZI1
             DO 11  J  =  1, NN
                Z(LZIJ)  =  CMPLX(REAL(C)*RANF() + REAL(A),
     +                      AIMAG(C)*RANF() + AIMAG(A))
                LZIJ     =  LZIJ + JZ
  11            CONTINUE
             LZI1  =  LZI1 + IZ
  12         CONTINUE
          RETURN
          END
