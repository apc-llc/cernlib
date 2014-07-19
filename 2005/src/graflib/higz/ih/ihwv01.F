*
* $Id: ihwv01.F,v 1.1.1.1 1996/02/14 13:10:54 mclareni Exp $
*
* $Log: ihwv01.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:54  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/09 14/01/92  14.01.24  by  O.Couet
*-- Author :
      SUBROUTINE IHWV01(RMIN,RMAX,SCALE,CENTER,IREP)
************************************************************************
*                                                                      *
*     IHWV01                                     Date:    13.08.90     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised: 08.01.92     *
*                                                                      *
*     Function: Find centre of a MIN-MAX scope and scale factors       *
*                                                                      *
*     Input: RMIN(3) - MIN -                                           *
*            RMAX(3) -       MAX scope                                 *
*                                                                      *
*     Output: SCALE(3)  - scale factors                                *
*             CENTER(3) - centre                                       *
*             IREP      - reply (-1 if error in min-max)               *
*                                                                      *
************************************************************************
      REAL              RMIN(3),RMAX(3),SCALE(3),CENTER(3)
*-
      IREP   = 0
      SQRT3  = SQRT(3.)/2.
      DO 100 I=1,3
        IF (RMIN(I) .GE. RMAX(I))       GOTO 998
        SCALE(I)  = (RMAX(I)-RMIN(I)) * SQRT3
        CENTER(I) = (RMAX(I)+RMIN(I)) / 2.
  100 CONTINUE
      GOTO 999
*
  998 IREP   =-1
  999 RETURN
      END
