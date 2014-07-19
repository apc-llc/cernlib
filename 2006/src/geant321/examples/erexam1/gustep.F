*
* $Id: gustep.F,v 1.1.1.1 1996/03/06 15:36:25 mclareni Exp $
*
* $Log: gustep.F,v $
* Revision 1.1.1.1  1996/03/06 15:36:25  mclareni
* Add geane321 examples
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.50  by  S.Giani
*-- Author :
      SUBROUTINE GUSTEP
*
*     User routine called at the end of each tracking step
*
#include "geant321/gcflag.inc"
*
* *** Debug event
      IF (IDEBUG*ISWIT(2).NE.0) CALL GPCXYZ
*
      END
