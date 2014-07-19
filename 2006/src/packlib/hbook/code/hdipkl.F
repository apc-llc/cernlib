*
* $Id: hdipkl.F,v 1.1.1.1 1996/01/16 17:07:35 mclareni Exp $
*
* $Log: hdipkl.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:35  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
      FUNCTION HDIPKL(Z)
*==========>
C         CALCULATES THE PROBABILITY OF EXCEEDING THE VALUE Z=N**2 * DN
C         FOR THE KOLMOGOROV TEST, WHERE DN=MAX DISTANCE BETWEEN
C         CUMULATIVE DISTRIBUTION FUNCTION AND N EXPERIMENTAL VALUES.
C          FUNCTION HOLDS ONLY FOR LARGE N, BUT IS ACCURATE TO 10**-11
C     THETA FUNCTIONS INVERSION FORMULA IS USED FOR THE ARGUMENTS .LE.1
C
C            This is a copy of the CERN Library routine PROBKL
*==========>
      DIMENSION CONS(3) , FJ2(5)
      SAVE CONS,FJ2,SQR2PI
C         CONS(J) = -0.5*(PI*(2*J-1)/2)**2
      DATA CONS / -1.233700550136 , -11.10330496 , -30.84251376 /
C         FJ2(J) = -2 * J**2
      DATA FJ2 / -2. , -8. , -18. , -32. , -50. /
      DATA SQR2PI/2.50662827463/
      P = 0.
      IF(Z.GT.1.) GO TO 3
      IF(Z.LT.0.2) GO TO 6
C         Z .LT. 1.     USE SERIES IN EXP(1/Z**2)
      ZINV = 1./Z
      A = SQR2PI * ZINV
      ZINV2 = ZINV**2
      DO 4 J= 1, 3
         ARG = CONS(J)*ZINV2
         IF(ARG.LT.-30.)GO TO 4
         P = P + EXP(ARG)
    4 CONTINUE
      P = 1. - A*P
      GO TO 2
C         Z .GT. 1    USE SERIES IN EXP(Z**2)
    3 SIG2 = -2.
      Z2 = Z**2
      DO 5 J= 1, 5
         SIG2 = -SIG2
         C = FJ2(J) * Z2
         IF(C.LT.-100) GO TO 2
         E = SIG2 * EXP(C)
    5 P = P + E
      GO TO 2
C         Z .LT. 0.2             PROB = 1.
    6 P=1.
    2 CONTINUE
      HDIPKL = P
      RETURN
      END
