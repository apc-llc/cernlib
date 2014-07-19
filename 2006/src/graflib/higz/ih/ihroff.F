*
* $Id: ihroff.F,v 1.1.1.1 1996/02/14 13:10:53 mclareni Exp $
*
* $Log: ihroff.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:53  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/05 26/08/93  09.53.55  by  O.Couet
*-- Author :
      SUBROUTINE IHROFF()
************************************************************************
*                                                                      *
*     IHROFF                                     Date:    16.05.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Switch OFF "RASTER SCREEN" algorithm                   *
*                                                                      *
************************************************************************
#include "higz/hcrast.inc"
*-
      IFRAST = 0
      END
