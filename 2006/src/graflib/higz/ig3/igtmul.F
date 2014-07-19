*
* $Id: igtmul.F,v 1.1.1.1 1996/02/14 13:10:42 mclareni Exp $
*
* $Log: igtmul.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:42  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  2.07/20 19/12/95  17.23.24  by  O.Couet
*-- Author :
      SUBROUTINE IGTMUL(T1,T2,T)
************************************************************************
*                                                                      *
* Name: IGTMUL                                     Date:    04.12.95   *
* Author: E.Chernyaev (IHEP/Protvino)              Revised:            *
*                                                                      *
* Function: Multiply two transformation matrixes                       *
*                                                                      *
************************************************************************
      REAL T1(4,3),T2(4,3),T(4,3)
      DOUBLE PRECISION DT1(4,4),DT2(4,3)
*-
      DO 120 I=1,4
        DO 110 K=1,3
          DT1(I,K) = T1(I,K)
          DT2(I,K) = T2(I,K)
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
      END
