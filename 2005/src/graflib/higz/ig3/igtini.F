*
* $Id: igtini.F,v 1.1.1.1 1996/02/14 13:10:42 mclareni Exp $
*
* $Log: igtini.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:42  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  2.07/20 19/12/95  17.23.24  by  O.Couet
*-- Author :
      SUBROUTINE IGTINI(T)
************************************************************************
*                                                                      *
* Name: IGTINI                                     Date:    04.12.95   *
* Author: E.Chernyaev (IHEP/Protvino)              Revised:            *
*                                                                      *
* Function: Initialize matrix T by the identity transformation         *
*                                                                      *
************************************************************************
      REAL T(4,3)
*-
      DO 200 I=1,3
        DO 100 K=1,4
          T(K,I) = 0.
  100   CONTINUE
        T(I,I) = 1.
  200 CONTINUE
      END
