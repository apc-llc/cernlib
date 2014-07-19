*
* $Id: gutrev.F,v 1.1.1.1 1995/10/24 10:22:13 cernlib Exp $
*
* $Log: gutrev.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:13  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE GUTREV
C.
C.    ******************************************************************
C.    *                                                                *
C.    *                                                                *
C.    *       User routine to control tracking of one event            *
C.    *                                                                *
C.    *       Called by GRUN                                           *
C.    *                                                                *
C.    *                                                                *
C.    ******************************************************************
C.
C.
C.    ------------------------------------------------------------------
C.
#include "geant321/gcbank.inc"
#include "geant321/gclist.inc"
#include "geant321/gcflag.inc"
C
      IF(NGET.EQ.0)GO TO 10
      CALL GLOOK('HITS',LGET,NGET,ILOOK)
      IF(ILOOK.NE.0)GO TO 99
C
C
  10  CALL GTREVE
C
  99  RETURN
      END
