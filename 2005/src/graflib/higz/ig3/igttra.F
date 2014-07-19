*
* $Id: igttra.F,v 1.1.1.1 1996/02/14 13:10:43 mclareni Exp $
*
* $Log: igttra.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:43  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  2.07/20 19/12/95  17.23.24  by  O.Couet
*-- Author :
      SUBROUTINE IGTTRA(DX,DY,DZ,T)
************************************************************************
*                                                                      *
* Name: IGTTRA                                     Date:    04.12.95   *
* Author: E.Chernyaev (IHEP/Protvino)              Revised:            *
*                                                                      *
* Function: Add to matrix T the translation by DX, DY, DZ              *
*                                                                      *
************************************************************************
      REAL T(4,3)
*-
      T(4,1) = T(4,1) + DX
      T(4,2) = T(4,2) + DY
      T(4,3) = T(4,3) + DZ
      END
