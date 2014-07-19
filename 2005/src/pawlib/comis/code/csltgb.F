*
* $Id: csltgb.F,v 1.1.1.1 1996/02/26 17:16:17 mclareni Exp $
*
* $Log: csltgb.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:17  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 05/10/93  15.03.29  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSLTGB(IPVS)
***---------------------------------
*    a set of routines CS%T%% for dealing
*    this comis tables
***---------------------------------
      INTEGER CSLTAB
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
      IP=IPGB
      I=CSLTAB(IP,IPVS,KSLENB)
      IF(I.NE.0)THEN
        CALL CCOPYA(IQ(I+2),IADGB,KSIDB-2)
      ELSE
*        CALL VFILL(IADGB,KSIDB-2,0)
        CALL VZERO(IADGB,KSIDB-2)
      ENDIF
      CSLTGB=I
      END
