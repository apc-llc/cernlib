*
* $Id: gutrev.F,v 1.1.1.1 1995/10/24 10:22:17 cernlib Exp $
*
* $Log: gutrev.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:17  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE GUTREV
*
************************************************************************
*                                                                      *
*             User routine to control tracking of one event            *
*                                                                      *
*             Called by GRUN (in GEANT, controls a run of events)      *
*                                                                      *
************************************************************************
*
#include "geant321/gcflag.inc"
*
*     -----------------------------------------------------------------
*..geant..(TRAK 110)..
      CALL GTREVE
C..              in turn calls GUTRAK which, if not here,
C..              is in GEANT3. GUTRAK calls GTRACK (steering routine
C..              to track a ptcl).
*
*             Debug and plot tracks.
*
      IF(IDEBUG.EQ.1) THEN
        IF(ISWIT(2).EQ.1) CALL GPRINT('JXYZ', 0)
        IF(ISWIT(3).EQ.1) THEN
C..geant..draw a view..
          CALL GDSHOW(1)
C..geant..draw a track (0 means all tracks)..
          CALL GDXYZ (0)
          CALL ICLRWK(0,0)
        ENDIF
      ENDIF
*
      END
