*
* $Id: cssems.F,v 1.1.1.1 1996/02/26 17:16:18 mclareni Exp $
*
* $Log: cssems.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:18  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.36  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSSEMS(N)
***--------------------------
*      works with semantic stack
***--------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cspnts.inc"
      IF(ITS+N.LT.ILSEM)RETURN
      IL=ILSEM-IBSEM
      IL=MAX0(IL+N,IL+50)
      I=MHLOC(IL)
      L=ITS-IBSEM
      CALL CCOPYA(IQ(IBSEM),IQ(I),L)
      CALL MHFREE(IBSEM)
      IBSEM=I
      ITS=I+L
      ILSEM=IBSEM+IL
      END
