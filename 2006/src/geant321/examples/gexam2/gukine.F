*
* $Id: gukine.F,v 1.2 1996/02/01 14:54:49 ravndal Exp $
*
* $Log: gukine.F,v $
* Revision 1.2  1996/02/01 14:54:49  ravndal
* Clean up the repository
*
* Revision 1.1.1.1  1995/10/24 10:22:11  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE GUKINE
*
************************************************************************
*                                                                      *
*             Read or Generates Kinematics for primary tracks          *
*                                                                      *
************************************************************************
*
#include "geant321/gcflag.inc"
#include "geant321/gckine.inc"
#include "cdhscom.inc"
      DIMENSION VERTEX(3),PLAB(3)
*
*     -----------------------------------------------------------------
*
      CALL GRANOR(RANX, RANY)
      VERTEX(1) = -60.0 + RANX * SIGBX
      VERTEX(2) = -80.0 + RANY * SIGBY
      VERTEX(3) = -165.999
      CALL GRANOR(RANP, DUMMY)
      PLAB(1) = 0.0
      PLAB(2) = 0.0
      PLAB(3) = PKINE(1) * (1. + SIGP * RANP)
*
      CALL GSVERT(VERTEX,0,0,0,0,NVERT)
      CALL GSKINE(PLAB,IKINE,NVERT,0,0,NT)
*
*             Reset energy deposited
*
      CALL VZERO(ESINT,128)
*
*              Kinematic debug (controled by ISWIT(1))
*
      IF(IDEBUG.EQ.1.AND.ISWIT(1).EQ.1) THEN
        CALL GPRINT('VERT',0)
        CALL GPRINT('KINE',0)
      ENDIF
*
      END
