*
* $Id: igtcop.F,v 1.1.1.1 1996/02/14 13:10:42 mclareni Exp $
*
* $Log: igtcop.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:42  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  2.07/20 19/12/95  17.23.24  by  O.Couet
*-- Author :
      SUBROUTINE IGTCOP(T1,T2)
************************************************************************
*                                                                      *
* Name: IGTCOP                                     Date:    04.12.95   *
* Author: E.Chernyaev (IHEP/Protvino)              Revised:            *
*                                                                      *
* Function: Copy transformation matrix T1 to transformation matrix T2  *
*                                                                      *
************************************************************************
      REAL T1(4,3),T2(4,3)
*-
      DO 200 K=1,4
        DO 100 I=1,3
          T2(K,I) = T1(K,I)
  100   CONTINUE
  200 CONTINUE
      END
