*
* $Id: gukine.F,v 1.2 1996/02/22 13:16:26 ravndal Exp $
*
* $Log: gukine.F,v $
* Revision 1.2  1996/02/22 13:16:26  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:19  cernlib
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
#include "shobuf.inc"
C
      DIMENSION VERTEX(3),PLAB(3)
      SAVE VERTEX,PLAB
      DATA VERTEX/3*0./
      DATA PLAB  /3*0./
C
C *** Energy of the incident particle
C
      ISHOW = ISHOW+1
      IF(ISHOW.GT.NSHOW(IENERG)) THEN
         IENERG = IENERG+1
         ISHOW  = 1
      ENDIF
C
      IF(ISHOW.EQ.1) THEN
         CALL VZERO(IENBUF,NWORD)
         JPA = LQ(JPART-IKINE)
         AMASS = Q(JPA+7)
         ETOT  = ESHOW(IENERG)
         PLAB(3)=SQRT((ETOT+AMASS)*(ETOT-AMASS))
          ENBUF(1)=ETOT*1000.
         IENBUF(2)=NSHOW(IENERG)
      ENDIF
C
C *** Vertex creation
C
      VERTEX(1) = XGENER
      VERTEX(2) = YGENER
      VERTEX(3) = ZGENER
C
      CALL GSVERT(VERTEX,0,0,0,0,NVERT)
      CALL GSKINE(PLAB,IKINE,NVERT,0,0,NT)
C
C *** Kinematics debug (controlled by ISWIT(1) )
C
      IF(IDEBUG*ISWIT(1).EQ.1) THEN
         CALL GPRINT('VERT',0)
         CALL GPRINT('KINE',0)
      ENDIF
C
      END
