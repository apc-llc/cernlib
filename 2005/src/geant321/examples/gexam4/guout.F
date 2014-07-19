*
* $Id: guout.F,v 1.2 1996/02/01 15:00:35 ravndal Exp $
*
* $Log: guout.F,v $
* Revision 1.2  1996/02/01 15:00:35  ravndal
* Clean up repository
*
* Revision 1.1.1.1  1995/10/24 10:22:14  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE GUOUT
*
************************************************************************
*                                                                      *
*             GEANT3 user routine called at the end of each event.     *
*                                                                      *
************************************************************************
*
*
#include "geant321/gcunit.inc"
#include "geant321/gcflag.inc"
#include "urmips.inc"
      SAVE N10
      DATA N10/0/
*
*     ------------------------------------------------------------------
*
      NTXMIP=NTXMIP+1
      TXMIP=TXMIP+XMIP
      TXMIP2=TXMIP2+XMIP*XMIP
      CALL HFF1(10,N10,XMIP,1.)
*
      IF(ISWIT(6).NE.0)THEN
         IS6=ISWIT(6)
         IF(MOD(IEVENT,IS6).EQ.0)CALL PXMIPS
      ENDIF
*
      END
