*
* $Id: gukine.F,v 1.2 1996/02/01 15:03:13 ravndal Exp $
*
* $Log: gukine.F,v $
* Revision 1.2  1996/02/01 15:03:13  ravndal
* Clean up repository
*
* Revision 1.1.1.1  1995/10/24 10:22:17  cernlib
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
*             Read or Generates Kinematics for primary tracks          *
*                                                                      *
************************************************************************
*
#include "geant321/gcflag.inc"
#include "geant321/gckine.inc"
#include "nascom.inc"
*
      DIMENSION VERTEX(3),PLAB(3)
*
*     -----------------------------------------------------------------
*
 
      VERTEX(1) = 0.
      VERTEX(2) = 0.
      VERTEX(3) = PKINE(2)
C
      PLAB(1) = 0
      PLAB(2) = 0
      PLAB(3) = PKINE(1)
*
      CALL GSVERT(VERTEX,0,0,0,0,NVERT)
      CALL GSKINE(PLAB,IKINE,NVERT,0,0,NT)
*
*             Reset energy deposited
*
      ED1=0.
      ED2=0.
      ETOT=0.
*
*              Kinematic debug (controled by ISWIT(1))
*
      IF(IDEBUG.EQ.1.AND.ISWIT(1).EQ.1) THEN
        CALL GPRINT('VERT',0)
        CALL GPRINT('KINE',0)
      ENDIF
*
      END
