*
* $Id: ihrcle.F,v 1.1.1.1 1996/02/14 13:10:53 mclareni Exp $
*
* $Log: ihrcle.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:53  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/05 26/05/93  17.59.17  by  O.Couet
*-- Author :
      SUBROUTINE IHRCLE()
************************************************************************
*                                                                      *
*     IHRCLE                                     Date:    16.05.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Clear screen ("RASTER SCREEN")                         *
*                                                                      *
************************************************************************
#include "higz/hcrast.inc"
*-
      NW     = (NXRAST*NYRAST + NBW - 1) / NBW
      DO 200 I=1,NW
        IRAST(JRAST+I) = 0
  200 CONTINUE
      IFRAST = 1
      RETURN
      END
