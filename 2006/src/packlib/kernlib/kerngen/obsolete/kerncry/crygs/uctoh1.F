*
* $Id: uctoh1.F,v 1.1.1.1 1996/02/15 17:52:40 mclareni Exp $
*
* $Log: uctoh1.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:40  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      SUBROUTINE UCTOH1 (VC,MVH,NCH)
C
C CERN PROGLIB# M409    UCTOH1          .VERSION KERNCRY  1.06  870527
C ORIG. 20/05/87  Federico Carminati + Julius Zoll
C
      CHARACTER*(*) VC
      DIMENSION    MVH(9)

      DATA IBLANK  / X'0020202020202020' /

      IF (NCH.LE.0)          RETURN

      DO 16  JCH=1,NCH
   16 MVH(JCH) = IBLANK .OR. SHIFT(ICHAR(VC(JCH:JCH)), 56)
      RETURN
      END
#ifdef CERNLIB_TCGEN_UCTOH1
#undef CERNLIB_TCGEN_UCTOH1
#endif
