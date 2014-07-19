*
* $Id: igtnor.F,v 1.1.1.1 1996/02/14 13:10:43 mclareni Exp $
*
* $Log: igtnor.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:43  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  2.07/20 19/12/95  17.23.24  by  O.Couet
*-- Author :
      SUBROUTINE IGTNOR(ANORM1,T,ANORM2)
************************************************************************
*                                                                      *
* Name: IGTNOR                                     Date:    04.12.95   *
* Author: E.Chernyaev (IHEP/Protvino)              Revised:            *
*                                                                      *
* Function: Transform normal ANORM1 to normal ANORM2 using matrix T    *
*                                                                      *
************************************************************************
      REAL ANORM1(3),ANORM2(3),T(4,3)
      DOUBLE PRECISION X,Y,Z,A1,A2,A3,B1,B2,B3,C1,C2,C3
*-
      X  = ANORM1(1)
      Y  = ANORM1(2)
      Z  = ANORM1(3)
      A1 = T(1,1)
      A2 = T(2,1)
      A3 = T(3,1)
      B1 = T(1,2)
      B2 = T(2,2)
      B3 = T(3,2)
      C1 = T(1,3)
      C2 = T(2,3)
      C3 = T(3,3)
      ANORM2(1) = X*(B2*C3-B3*C2) + Y*(B3*C1-B1*C3) + Z*(B1*C2-B2*C1)
      ANORM2(2) = X*(C2*A3-C3*A2) + Y*(C3*A1-C1*A3) + Z*(C1*A2-C2*A1)
      ANORM2(3) = X*(A2*B3-A3*B2) + Y*(A3*B1-A1*B3) + Z*(A1*B2-A2*B1)
      END
