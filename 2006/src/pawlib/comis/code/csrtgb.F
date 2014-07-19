*
* $Id: csrtgb.F,v 1.1.1.1 1996/02/26 17:16:17 mclareni Exp $
*
* $Log: csrtgb.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:17  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 01/10/93  16.03.31  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSRTGB(I)
***--------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
      CALL CCOPYA(IADGB,IQ(I+2),KSLENB-1)
      END
