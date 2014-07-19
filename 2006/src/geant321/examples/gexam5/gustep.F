*
* $Id: gustep.F,v 1.2 1996/02/01 15:03:15 ravndal Exp $
*
* $Log: gustep.F,v $
* Revision 1.2  1996/02/01 15:03:15  ravndal
* Clean up repository
*
* Revision 1.1.1.1  1995/10/24 10:22:17  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE GUSTEP
*
************************************************************************
*                                                                      *
*             User routine called at the end of each tracking step     *
*             MEC   is the mechanism origin of the step                *
*             INWVOL is different from 0 when the track has reached    *
*                    a volume boundary                                 *
*             ISTOP is different from 0 if the track has stopped       *
*                                                                      *
************************************************************************
*
#include "geant321/gctmed.inc"
#include "geant321/gcking.inc"
#include "geant321/gcflag.inc"
#include "geant321/gctrak.inc"
#include "nascom.inc"
*
*     -----------------------------------------------------------------
*
*             Accumulate energy deposited in silicon
*
      IF     (NUMED.EQ.2)THEN
         IF(DESTEP.GT.0.)THEN
            ED1=ED1+DESTEP
            ETOT=ETOT+DESTEP
         ENDIF
      ELSEIF (NUMED.EQ.3)THEN
         IF(DESTEP.GT.0.)THEN
            ED2=ED2+DESTEP
            ETOT=ETOT+DESTEP
         ENDIF
      ENDIF
*
*             Something generated ?
*
      IF(NGKINE.GT.0) THEN
          CALL GSKING(0)
      ENDIF
*
*             Debug/plot event
      IF(IDEBUG.EQ.1) THEN
        IF((ISWIT(2).EQ.1).OR.(ISWIT(3).EQ.1)) CALL GSXYZ
        IF (ISWIT(2).EQ.2) CALL GPCXYZ
        IF (ISWIT(2).EQ.3) CALL GDCXYZ
      ENDIF
      END
