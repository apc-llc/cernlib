*
* $Id: gutrev.F,v 1.1.1.1 1995/10/24 10:22:11 cernlib Exp $
*
* $Log: gutrev.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:11  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE GUTREV
*
************************************************************************
*                                                                      *
*             User routine to control tracking of one event            *
*                                                                      *
*             Called by GRUN                                           *
*                                                                      *
************************************************************************
*
#include "geant321/gcflag.inc"
*
*     -----------------------------------------------------------------
*
      CALL GTREVE
*
*             Debug and plot tracks.
*
      IF(IDEBUG.EQ.1) THEN
        IF(ISWIT(2).EQ.1) CALL GPRINT('JXYZ', 0)
#if defined(CERNLIB_HIGZ)
        IF(ISWIT(3).EQ.1) THEN
          CALL GDSHOW(1)
          CALL GDXYZ (0)
          CALL ICLRWK(0,0)
        ENDIF
#endif
      ENDIF
*
      END
