*
* $Id: csrtll.F,v 1.1.1.1 1996/02/26 17:16:18 mclareni Exp $
*
* $Log: csrtll.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:18  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.36  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSRTLL(I)
***--------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstab.inc"
      CALL CCOPYA(LABLL,IQ(I+1),3)
      END
