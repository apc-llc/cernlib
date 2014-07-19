*
* $Id: ubunch.F,v 1.1.1.1 1996/02/15 17:52:39 mclareni Exp $
*
* $Log: ubunch.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:39  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
C
C CERN PROGLIB# M409    UBUNCH          .VERSION KERNCRY  1.06  870527
C ORIG. 12/06/83 JZ, 20/05/87 remove entry UH1TOC
C
      SUBROUTINE UBUNCH (MS,MT,NCHP)

      DIMENSION    MS(99), MT(99)
      DATA  IBLANK / 8H         /

      JS  = 1
      NCH = NCHP
      IF (NCH.LE.0)          RETURN

      NW  = (NCH-1)/8 + 1
      NWL = NCH/8
      IF (NCH.LT.8)          GO TO 31

C--                PACK THE LEADING COMPLETE WORDS

      DO  24    JW=1,NWL
      MM =                  SHIFTR(MS(JS+0),56)
      MM = SHIFT(MM,8) .OR. SHIFTR(MS(JS+1),56)
      MM = SHIFT(MM,8) .OR. SHIFTR(MS(JS+2),56)
      MM = SHIFT(MM,8) .OR. SHIFTR(MS(JS+3),56)
      MM = SHIFT(MM,8) .OR. SHIFTR(MS(JS+4),56)
      MM = SHIFT(MM,8) .OR. SHIFTR(MS(JS+5),56)
      MM = SHIFT(MM,8) .OR. SHIFTR(MS(JS+6),56)
      MM = SHIFT(MM,8) .OR. SHIFTR(MS(JS+7),56)
      MT(JW) = MM
   24 JS = JS + 8

      IF (NWL.EQ.NW)         RETURN

C--                PACK THE TRAILING WORD

CDIR$ NOVECTOR

   31 N  = NCH+1 - JS

      DO  34  J=JS,NCH
   34 MM = SHIFTL(MM,8) .OR. SHIFTR(MS(J),56)

      MT(NW) = SHIFTL(MM,8*(8-N)) .OR. SHIFTR(IBLANK,8*N)
      RETURN
      END
#ifdef CERNLIB_TCGEN_UBUNCH
#undef CERNLIB_TCGEN_UBUNCH
#endif
