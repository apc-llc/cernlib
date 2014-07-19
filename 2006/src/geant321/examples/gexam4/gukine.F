*
* $Id: gukine.F,v 1.2 1996/02/01 15:00:34 ravndal Exp $
*
* $Log: gukine.F,v $
* Revision 1.2  1996/02/01 15:00:34  ravndal
* Clean up repository
*
* Revision 1.1.1.1  1995/10/24 10:22:14  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE GUKINE
*
************************************************************************
*                                                                      *
*             GEANT3 user routine to generate Kinematics               *
*               for primary tracks                                     *
*                                                                      *
************************************************************************
*
#include "geant321/gcflag.inc"
#include "geant321/gckine.inc"
#include "geant321/gconsp.inc"
#include "urgeom.inc"
#include "urmips.inc"
      DIMENSION VERTEX(6),PLAB(3),RNDM(2)
*
*     -----------------------------------------------------------------
*
      XMIP=0.
      CALL VZERO(VERTEX,6)
      VERTEX(1)=0.04
      VERTEX(2)=0.04
      VERTEX(3)=-HLCALO+.01
      IF(IKINE.GT.100)THEN
         IK=IKINE-100
         THETA=PKINE(2)*DEGRAD
         PHI=PKINE(3)*DEGRAD
      ELSE
         IK=IKINE
         CALL GRNDM(RNDM,2)
         THETA=PI*RNDM(1)
         PHI=TWOPI*RNDM(2)
      ENDIF
C
      PLAB(1) = PKINE(1)*SIN(THETA)*COS(PHI)
      PLAB(2) = PKINE(1)*SIN(THETA)*SIN(PHI)
      PLAB(3) = PKINE(1)*COS(THETA)
C
      CALL GSVERT(VERTEX,0,0,0,0,NVERT)
      CALL GSKINE(PLAB,IK,NVERT,0,0,NT)
*
*              Kinematic debug (controled by ISWIT(1))
*
      IF(IDEBUG.EQ.1.AND.ISWIT(1).EQ.1) THEN
        CALL GPRINT('VERT',0)
        CALL GPRINT('KINE',0)
      ENDIF
*
      END
