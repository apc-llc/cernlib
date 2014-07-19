*
* $Id: csldli.F,v 1.1.1.1 1996/02/26 17:16:18 mclareni Exp $
*
* $Log: csldli.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:18  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 04/10/93  12.03.59  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSLDLI(I)
***--------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
      CALL CCOPYA(IQ(I+1),NCIDGI,KSIDL-1)
      END
