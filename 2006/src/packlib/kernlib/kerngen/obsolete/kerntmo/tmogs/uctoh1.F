*
* $Id: uctoh1.F,v 1.1.1.1 1996/02/15 17:53:50 mclareni Exp $
*
* $Log: uctoh1.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:50  mclareni
* Kernlib
*
*
#include "kerntmo/pilot.h"
      SUBROUTINE UCTOH1 (KCHAR,IHOLL,NCH)
C
C CERN PROGLIB# M409    UCTOH1          .VERSION KERNTMO  1.00  890208
C ORIG. 3/02/89 K.M.STORR
C
      CHARACTER*(*) KCHAR
      CHARACTER*1 CHBUF(400)
      INTEGER*4    IHOLL(100),NCH
      INTEGER*4    HOLPAS(100)
      EQUIVALENCE  (HOLPAS(1),CHBUF(1))
C
      DO 1 JED = 1, NCH
         CHBUF( JED ) = KCHAR( JED:JED )
 1    CONTINUE
C
      CALL UBLOW (HOLPAS,IHOLL,NCH)
      RETURN
      END
#ifdef CERNLIB_TCGEN_UCTOH1
#undef CERNLIB_TCGEN_UCTOH1
#endif
