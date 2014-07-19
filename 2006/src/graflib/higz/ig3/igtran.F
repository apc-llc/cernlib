*
* $Id: igtran.F,v 1.1.1.1 1996/02/14 13:10:43 mclareni Exp $
*
* $Log: igtran.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:43  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  2.07/20 19/12/95  17.23.24  by  O.Couet
*-- Author :
      SUBROUTINE IGTRAN(P1,T,P2)
************************************************************************
*                                                                      *
* Name: IGTRAN                                     Date:    04.12.95   *
* Author: E.Chernyaev (IHEP/Protvino)              Revised:            *
*                                                                      *
* Function: Transform point P1 to point P2 using matrix T              *
*                                                                      *
************************************************************************
      REAL P1(3),P2(3),T(4,3)
      DOUBLE PRECISION P(3)
*-
      P(1) = P1(1)
      P(2) = P1(2)
      P(3) = P1(3)
      DO 100 I=1,3
        P2(I) = T(1,I)*P(1) + T(2,I)*P(2) + T(3,I)*P(3) + T(4,I)
  100 CONTINUE
      END
