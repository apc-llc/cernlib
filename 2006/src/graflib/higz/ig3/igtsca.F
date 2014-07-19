*
* $Id: igtsca.F,v 1.1.1.1 1996/02/14 13:10:43 mclareni Exp $
*
* $Log: igtsca.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:43  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  2.07/20 19/12/95  17.23.24  by  O.Couet
*-- Author :
      SUBROUTINE IGTSCA(SX,SY,SZ,T)
************************************************************************
*                                                                      *
* Name: IGTSCA                                     Date:    04.12.95   *
* Author: E.Chernyaev (IHEP/Protvino)              Revised:            *
*                                                                      *
* Function: Add to matrix T the scaling transformation with factors    *
*           SX, SY, SZ                                                 *
*                                                                      *
************************************************************************
      REAL T(4,3)
*-
      DO 100 I=1,4
        T(I,1) = T(I,1) * SX
        T(I,2) = T(I,2) * SY
        T(I,3) = T(I,3) * SZ
  100 CONTINUE
      END
