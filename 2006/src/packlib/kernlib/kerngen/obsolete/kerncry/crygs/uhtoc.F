*
* $Id: uhtoc.F,v 1.1.1.1 1996/02/15 17:52:40 mclareni Exp $
*
* $Log: uhtoc.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:40  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      SUBROUTINE UHTOC (MVH,NHL,VC,NCH)
C
C CERN PROGLIB# M409    UHTOC           .VERSION KERNCRY  1.06  870527
C ORIG. 20/05/87  Federico Carminati + Julius Zoll
C
      CHARACTER*(*) VC
      DIMENSION    MVH(9)

      IF (NCH.LE.0)          RETURN

      NHPW  = MIN(NHL,8)
      NFULL = NCH / NHPW
      NREST = MOD (NCH,NHPW)
      JCH   = 0
      IF (NFULL.EQ.0)        GO TO 21

C--                Do the complete words

      DO 17  JWORD=1,NFULL
      IWD = MVH(JWORD)

      DO 16  J=1,NHPW
      IWD = SHIFT(IWD,8)
      ICH = IWD .AND. MASK(128-8)
      JCH = JCH + 1
   16 VC(JCH:JCH) = CHAR(ICH)
   17 CONTINUE
      IF (NREST.EQ.0)        RETURN

C--                Do the last incomplete word

   21 IWD = MVH(NFULL+1)

      DO 24  J=1,NREST
      IWD = SHIFT(IWD,8)
      ICH = IWD .AND. MASK(128-8)
      JCH = JCH + 1
   24 VC(JCH:JCH) = CHAR(ICH)
      RETURN
      END
#ifdef CERNLIB_TCGEN_UHTOC
#undef CERNLIB_TCGEN_UHTOC
#endif
