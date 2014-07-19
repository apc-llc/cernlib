*
* $Id: ihsini.F,v 1.1.1.1 1996/02/14 13:10:53 mclareni Exp $
*
* $Log: ihsini.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:53  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.14/00 19/02/92  10.16.08  by  O.Couet
*-- Author :
      SUBROUTINE IHSINI(XMIN,XMAX)
************************************************************************
*                                                                      *
*     IHSINI                                     Date:    07.08.90     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Initialise "MOVING SCREEN" method                      *
*                                                                      *
*     Input: XMIN - left boundary                                      *
*            XMAX - right boundary                                     *
*                                                                      *
************************************************************************
#include "higz/hcscrn.inc"
*-
      X0     = XMIN
      DX     = (XMAX-XMIN) / NSLIC
      DO 100 I=1,NSLIC
        U(1,I) =-999.
        U(2,I) =-999.
        D(1,I) =+999.
        D(2,I) =+999.
  100   CONTINUE
      RETURN
      END
