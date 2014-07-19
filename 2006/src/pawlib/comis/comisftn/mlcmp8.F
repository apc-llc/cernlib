*
* $Id: mlcmp8.F,v 1.1.1.1 1996/02/26 17:16:48 mclareni Exp $
*
* $Log: mlcmp8.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:48  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.12/05 20/03/92  16.04.19  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLCMP8(C1,C2)
#if (defined(CERNLIB_APOLLO)||defined(CERNLIB_IBM)||defined(CERNLIB_UNIX))&&(!defined(CERNLIB_DECS))
      INTEGER C1(2),C2(2)
      CHARACTER*8 CH1,CH2
      JP=MJCHAR(C1)
      J=MJSCHA(CH1)
      CALL CCOPYS(JP,J,8)
      JP=MJCHAR(C2)
      J=MJSCHA(CH2)
      CALL CCOPYS(JP,J,8)
      IF(CH1.EQ.CH2)MLCMP8=0
      IF(CH1.GT.CH2)MLCMP8=1
      IF(CH1.LT.CH2)MLCMP8=-1
#endif
#if defined(CERNLIB_VAX)||defined(CERNLIB_DECS)
      BYTE C1(8),C2(8)
      DO 1 I=1,8
      IF(C1(I).GT.C2(I))GO TO 2
      IF(C1(I).LT.C2(I))GO TO 3
  1   CONTINUE
      MLCMP8=0
      RETURN
  2   MLCMP8=1
      RETURN
  3   MLCMP8=-1
#endif
      END
