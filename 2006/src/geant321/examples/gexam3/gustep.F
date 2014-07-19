*
* $Id: gustep.F,v 1.1.1.1 1995/10/24 10:22:13 cernlib Exp $
*
* $Log: gustep.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:13  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE GUSTEP
C.
C.    ******************************************************************
C.    *                                                                *
C.    *                                                                *
C.    *       User routine called at the end of each tracking step     *
C.    *       INWVOL is different from 0 when the track has reached    *
C.    *              a volume boundary                                 *
C.    *       ISTOP is different from 0 if the track has stopped       *
C.    *                                                                *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcflag.inc"
#include "geant321/gctmed.inc"
#include "geant321/gckine.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcsets.inc"
#include "geant321/gcking.inc"
      DIMENSION HITS(9)
C.
C.    ------------------------------------------------------------------
C.
      IF(IDET.EQ.0)GO TO 20
      IF(CHARGE.EQ.0.)GO TO 20
      IF(INWVOL.NE.1)GO TO 20
      DO 10 I=1,3
         HITS(I) =VECT(I)
   10 CONTINUE
      CALL GMTOD(VECT(1),HITS(4),1)
      CALL GMTOD(VECT(4),HITS(7),2)
      CALL GSAHIT(ISET,IDET,ITRA,NUMBV,HITS,IHIT)
C
   20 IF(ISWIT(6).EQ.0)THEN
C
         IF(NGKINE.GT.0)THEN
            DO 30 I=1,NGKINE
               IFLGK(I)=1
   30       CONTINUE
            CALL GSKING(0)
         ENDIF
      ENDIF
C
      CALL GDEBUG
C
  999 END
