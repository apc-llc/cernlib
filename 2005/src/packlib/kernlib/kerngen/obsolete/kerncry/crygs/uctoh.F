*
* $Id: uctoh.F,v 1.1.1.1 1996/02/15 17:52:39 mclareni Exp $
*
* $Log: uctoh.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:39  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      SUBROUTINE UCTOH (VC,MVH,NHL,NCH)
C
C CERN PROGLIB# M409    UCTOH           .VERSION KERNCRY  1.06  870527
C ORIG. 20/05/87  Federico Carminati + Julius Zoll
C
      CHARACTER*(*) VC
      DIMENSION    MVH(9), IBLV(8)

      DATA IBLV    / X'0020202020202020'
     +,              X'0000202020202020'
     +,              X'0000002020202020'
     +,              X'0000000020202020'
     +,              X'0000000000202020'
     +,              X'0000000000002020'
     +,              X'0000000000000020'
     +,              X'0000000000000000' /

      IF (NCH.LE.0)          RETURN

      NHPW  = MIN(NHL,8)
      NFULL = NCH / NHPW
      NREST = MOD (NCH,NHPW)
      JCH   = 0
      IF (NFULL.EQ.0)        GO TO 21

      IBLANK = IBLV(NHPW)

      DO 17  JWORD=1,NFULL
      IWD = IBLANK
      JSH = 64

      DO 16  J=1,NHPW
      JCH = JCH + 1
      JSH = JSH - 8
   16 IWD = IWD .OR. SHIFT(ICHAR(VC(JCH:JCH)), JSH)
   17 MVH(JWORD) = IWD
      IF (NREST.EQ.0)        RETURN

C--                Do the last incomplete word

   21 IWD = IBLV(NREST)
      JSH = 64

      DO 24  J=1,NREST
      JCH = JCH + 1
      JSH = JSH - 8
      IWD = IWD .OR. SHIFT(ICHAR(VC(JCH:JCH)), JSH)
   24 CONTINUE
      MVH(NFULL+1) = IWD
      RETURN
      END
#ifdef CERNLIB_TCGEN_UCTOH
#undef CERNLIB_TCGEN_UCTOH
#endif
