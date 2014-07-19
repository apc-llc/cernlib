*
* $Id: uh1toc.F,v 1.1.1.1 1996/02/15 17:53:50 mclareni Exp $
*
* $Log: uh1toc.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:50  mclareni
* Kernlib
*
*
#include "kerntmo/pilot.h"
      SUBROUTINE UH1TOC (IHOLL,KCHAR,NCH)
C
C CERN PROGLIB# M409    UH1TOC          .VERSION KERNTMO  1.00  890208
C ORIG. 03/02/89  K.M.STORR
C
      CHARACTER*(*) KCHAR
      CHARACTER*1 CHBUF(400)
      INTEGER*4    IHOLL(100),NCH
      INTEGER*4    HOLPAS(100)
      EQUIVALENCE  (CHBUF(1),HOLPAS(1))
C
      CALL UBUNCH (IHOLL,HOLPAS,NCH)
C
      DO 1 JED = 1, NCH
         KCHAR( JED:JED ) = CHBUF( JED )
 1    CONTINUE
C
      RETURN
      END
#ifdef CERNLIB_TCGEN_UH1TOC
#undef CERNLIB_TCGEN_UH1TOC
#endif
