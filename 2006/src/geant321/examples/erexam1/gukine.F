*
* $Id: gukine.F,v 1.1.1.1 1996/03/06 15:36:25 mclareni Exp $
*
* $Log: gukine.F,v $
* Revision 1.1.1.1  1996/03/06 15:36:25  mclareni
* Add geane321 examples
*
*
*      GEANT TRACKING
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.50  by  S.Giani
*-- Author :
      SUBROUTINE GUKINE
*
*     Generates Kinematic for primary track
*
*     Card Kine : itype  momentum  teta  phi
*
#include "geant321/gcflag.inc"
#include "geant321/gckine.inc"
#include "geant321/gconst.inc"
*
      DIMENSION VERTEX(3)
*
      VERTEX(1) = 0.
      VERTEX(2) = 0.
      VERTEX(3) = 0.
      PMOM      = PKINE(1)
      TETA      = PKINE(2)*DEGRAD
      PHI       = PKINE(3)*DEGRAD
*
      PKINE( 8) = PMOM * SIN(TETA) * COS(PHI)
      PKINE( 9) = PMOM * SIN(TETA) * SIN(PHI)
      PKINE(10) = PMOM * COS(TETA)
*
      CALL GSVERT(VERTEX,0,0,0,0,NVERT)
*
      CALL GSKINE(PKINE(8),IKINE,NVERT,0,0,NT)
*
* *** Kinematics debug (controlled by ISWIT(1) )
      IF(IDEBUG*ISWIT(1).EQ.1) THEN
          CALL GPRINT('VERT',0)
          CALL GPRINT('KINE',0)
      ENDIF
*
      END
