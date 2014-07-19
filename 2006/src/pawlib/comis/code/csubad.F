*
* $Id: csubad.F,v 1.1.1.1 1996/02/26 17:16:23 mclareni Exp $
*
* $Log: csubad.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:23  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.58  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSUBAD(L)
      COMMON/MDPOOL/IQ(99)
#include "comis/csfmt.inc"
      IF(LULIST+L.GT.LUBTOT)THEN
 1      LUBTOT=LUBTOT+512
        IF(LULIST+L.GT.LUBTOT)GO TO 1
        I=MHLOC(LUBTOT)
        CALL CCOPYA(IQ(IUBS),IQ(I),LULIST)
        CALL MHFREE(IUBS)
        IUBS=I
      ENDIF
      END
