*
* $Id: csrtgi.F,v 1.1.1.1 1996/02/26 17:16:18 mclareni Exp $
*
* $Log: csrtgi.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:18  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 01/10/93  16.08.45  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSRTGI(I)
***--------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
      CALL CCOPYA(ISHGI,IQ(I+2),KSIDG-2)
      END
