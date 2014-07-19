*
* $Id: csdtab.F,v 1.1.1.1 1996/02/26 17:16:17 mclareni Exp $
*
* $Log: csdtab.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:17  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 27/09/93  17.38.58  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSDTAB(IBEG,I,IDES)
***------------------------------------
*      clears tables
***------------------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstab.inc"
      INXT=I
      IPVS=0
      IT=IBEG
   1  IF(IT.EQ.I)GO TO 2
      IPVS=IT
      IT=IQ(IT)
      GO TO 1
   2  IF(IPVS.NE.0)IQ(IPVS)=0
   3  IT=INXT
      IF(IT.EQ.0)GO TO 4
      INXT=IQ(IT)
      IF(IDES.NE.0)THEN
            CALL CCOPYA(IQ(IT+1),NCIDGI,8)
            IF(MODEGI.GT.1)THEN
              CALL MHFREE(MODEGI-1)
            ELSEIF(MODEGI.EQ.-1)THEN
              IF(IABS(ITYPGI).EQ.3)CALL MHFREE(IVPAR)
            ENDIF
      ENDIF
      CALL MHFREE(IT)
      GO TO 3
   4  END
