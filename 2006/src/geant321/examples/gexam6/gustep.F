*
* $Id: gustep.F,v 1.1.1.1 1995/10/24 10:22:19 cernlib Exp $
*
* $Log: gustep.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:19  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE GUSTEP
C.    *
C.    *       User routine called at the end of each tracking step
C.    *
#include "geant321/gcflag.inc"
#include "geant321/gckine.inc"
#include "geant321/gcking.inc"
#include "geant321/gcsets.inc"
#include "geant321/gctrak.inc"
C
C *** Debug event
C
      IF(IDEBUG*ISWIT(2).EQ.1) CALL GPCXYZ
C
C *** Something generated ?
C
      IF(NGKINE.GT.0) CALL GSKING(0)
C
C *** Energy deposited
C
      IF(DESTEP.NE.0.)CALL GSCHIT(ISET,IDET,ITRA,NUMBV,DESTEP,1,IHIT)
C
      END
