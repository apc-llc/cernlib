*
* $Id: uginit.F,v 1.1.1.1 1996/03/06 15:36:26 mclareni Exp $
*
* $Log: uginit.F,v $
* Revision 1.1.1.1  1996/03/06 15:36:26  mclareni
* Add geane321 examples
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.50  by  S.Giani
*-- Author :
      SUBROUTINE UGINIT
*
*
      CALL GINIT
      CALL GFFGO
      CALL GZINIT
      CALL GPART
      CALL GMATE
      CALL UGEOM
      CALL GPHYSI
*
* *** Print control
      CALL GPRINT('TMED',0)
      CALL GPRINT('VOLU',0)
*
      END
