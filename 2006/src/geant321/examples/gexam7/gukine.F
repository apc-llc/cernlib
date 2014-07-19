*
* $Id: gukine.F,v 1.2 1996/02/22 13:24:01 ravndal Exp $
*
* $Log: gukine.F,v $
* Revision 1.2  1996/02/22 13:24:01  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:21  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE GUKINE
C.    *
C.    *       Generates Kinematics for primary track
C.    *
#include "geant321/gcbank.inc"
#include "geant321/gcflag.inc"
#include "geant321/gckine.inc"
#include "pvolum.inc"
#include "bootwk.inc"
#include "celoss.inc"
C
      DIMENSION VERTEX(3),PLAB(3)
      SAVE VERTEX,PLAB
      DATA VERTEX/3*0./
      DATA PLAB  /3*0./
C.
      CALL VZERO(DEDL,6*NBIN)
      CALL VZERO(ELEAK,3)
      VERTEX(3) = - Z1     + 0.01
      CALL GSVERT(VERTEX,0,0,0,0,NVERT)
C
      JPA = LQ(JPART-IKINE)
      XMASS = Q(JPA+7)
      PKINE(3) = XMASS + PKINE(1)
      EINCID = PKINE(3)
      PLAB(3) = SQRT(PKINE(1)*(PKINE(3)+XMASS))
C
      CALL GSKINE(PLAB,IKINE,NVERT,0,0,NT)
C
C ***          Kinematics debug (controlled by ISWIT(1) )
C
      IF(IDEBUG*ISWIT(1).EQ.1) THEN
          CALL GPRINT('VERT',0)
          CALL GPRINT('KINE',0)
      ENDIF
C
      END
