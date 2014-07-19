*
* $Id: igtref.F,v 1.1.1.1 1996/02/14 13:10:43 mclareni Exp $
*
* $Log: igtref.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:43  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  2.07/20 19/12/95  17.23.24  by  O.Couet
*-- Author :
      SUBROUTINE IGTREF(P,ANORM,T)
************************************************************************
*                                                                      *
* Name: IGTREF                                     Date:    04.12.95   *
* Author: E.Chernyaev (IHEP/Protvino)              Revised:            *
*                                                                      *
* Function: Add to matrix T the reflection in a plane going though     *
*           point P and whose normal is equal to ANORM                 *
*                                                                      *
************************************************************************
      REAL P(3),ANORM(3),T(4,3)
      DOUBLE PRECISION A,B,C,D,S,DT1(4,4),DT2(4,3)
*-
      A      = ANORM(1)
      B      = ANORM(2)
      C      = ANORM(3)
      S      = DSQRT(A*A+B*B+C*C)
      SS     = S
      IF (SS .EQ. 0.) GOTO 999
      A      = A/S
      B      = B/S
      C      = C/S
      D      = -(A*P(1)+B*P(2)+C*P(3))
*
      DT2(1,1) = -A*A + B*B + C*C
      DT2(2,1) = -2*A*B
      DT2(3,1) = -2*A*C
      DT2(4,1) = -2*A*D
*
      DT2(1,2) = -2*A*B
      DT2(2,2) = A*A - B*B + C*C
      DT2(3,2) = -2*B*C
      DT2(4,2) = -2*B*D
*
      DT2(1,3) = -2*A*C
      DT2(2,3) = -2*B*C
      DT2(3,3) = A*A + B*B - C*C
      DT2(4,3) = -2*C*D
      DO 120 I=1,4
*
        DO 110 K=1,3
          DT1(I,K) = T(I,K)
  110   CONTINUE
        DT1(I,4) = 0.
  120 CONTINUE
      DT1(4,4) = 1.
*
      DO 220 I=1,4
        DO 210 K=1,3
          T(I,K) = DT1(I,1)*DT2(1,K) + DT1(I,2)*DT2(2,K) +
     +             DT1(I,3)*DT2(3,K) + DT1(I,4)*DT2(4,K)
  210   CONTINUE
  220 CONTINUE
*
  999 RETURN
      END
