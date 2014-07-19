*
* $Id: ublow.F,v 1.1.1.1 1996/02/15 17:52:38 mclareni Exp $
*
* $Log: ublow.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:38  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      SUBROUTINE UBLOW (MS,MT,NCHP)
C
C CERN PROGLIB# M409    UBLOW           .VERSION KERNCRY  1.06  870527
C ORIG. 12/06/83 JZ, 20/05/87 remove entry UCTOH1
C

      DIMENSION    MS(99), MT(99)
      DATA  IBLANK / '       'R /

      JT  = 1
      NCH = NCHP
      IF (NCH.LE.0)          RETURN

      NW  = (NCH-1)/8 + 1
      IF (NCH.LT.9)          GO TO 31

C--                UNPACK THE INITIAL COMPLETE WORDS

      NWL = NCH/8
      NCHL= 8*NWL

      CALL UNPACK (MS,8,MT,NCHL)

      DO 24  J=1,NCHL
   24 MT(J) = SHIFT(MT(J),56) .OR. IBLANK

      IF (NWL.EQ.NW)         RETURN
      JT  = NCHL + 1

C--                UNPACK THE LAST INCOMPLETE WORD

CDIR$ NOVECTOR

   31 M8  = MASK (8)
      MM  = MS(NW)

      DO 34  J=JT,NCH
      MT(J) = (MM.AND.M8) .OR. IBLANK
   34 MM    = SHIFT (MM,8)
      RETURN
      END
#ifdef CERNLIB_TCGEN_UBLOW
#undef CERNLIB_TCGEN_UBLOW
#endif
