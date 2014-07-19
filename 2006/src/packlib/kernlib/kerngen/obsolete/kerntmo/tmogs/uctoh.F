*
* $Id: uctoh.F,v 1.1.1.1 1996/02/15 17:53:50 mclareni Exp $
*
* $Log: uctoh.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:50  mclareni
* Kernlib
*
*
#include "kerntmo/pilot.h"
      SUBROUTINE UCTOH (KCHAR,IHOLL,NPW,NCH)
C
C CERN PROGLIB# M409    UCTOH           .VERSION KERNTMO  1.00  890208
C ORIG. 3/02/89  K.M.STORR
C
      CHARACTER*(*) KCHAR
      CHARACTER*1 CHBUF(400)
      INTEGER*4    IHOLL(100),NCH, NPW(100)
      INTEGER*4    HOLPAS(100)
      EQUIVALENCE  (CHBUF(1),HOLPAS(1))
C
      DO 1 JED = 1, NCH
         CHBUF( JED ) = KCHAR( JED:JED )
 1    CONTINUE
C
      CALL UTRANS (HOLPAS,IHOLL,NCH,99,NPW)
      RETURN
      END
#ifdef CERNLIB_TCGEN_UCTOH
#undef CERNLIB_TCGEN_UCTOH
#endif
