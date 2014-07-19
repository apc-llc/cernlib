*
* $Id: csbuse.F,v 1.1.1.1 1996/02/26 17:16:19 mclareni Exp $
*
* $Log: csbuse.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:19  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.36  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSBUSE(NB)
***--------------------------
*      routine for USE statement of comis
***---------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cspnts.inc"
      IF(NB.LT.0)THEN
        NBB=NB
      ELSEIF(NB.EQ.0)THEN
        NBB=NBG
      ELSE
        NBB=NBB+1
        IF(NBB.GT.LBB)THEN
          LBB=LBB+10
          I=MHLOC(LBB)
          IBB=IBB+1
          CALL CCOPYA(IQ(IBB),IQ(I),NBB)
          CALL MHFREE(IBB)
          IBB=I-1
        ENDIF
        IQ(IBB+NBB)=NB
      ENDIF
      END
