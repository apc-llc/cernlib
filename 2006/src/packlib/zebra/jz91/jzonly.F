*
* $Id: jzonly.F,v 1.2 1999/06/18 13:29:58 couet Exp $
*
* $Log: jzonly.F,v $
* Revision 1.2  1999/06/18 13:29:58  couet
* - qcardl.inc was empty: It is now removed and not used.
*   Comment END CDE removed.
*
* Revision 1.1.1.1  1996/03/06 10:47:17  mclareni
* Zebra
*
*
#include "zebra/pilot.h"
      SUBROUTINE JZONLY (IDV)

C-    PURGE REDUNDANT PROCESSORS TITLES

#include "zebra/mqsysh.inc"
#include "zebra/jzuc.inc"
#include "zebra/jzc.inc"
*
      DIMENSION    IDV(99)

#include "zebra/qstorjz.inc"

      NID = IDV(1)
      IFL = 0
      L   = LQJZ - 1

   21 L = LQ(KQS+L)
      IF (L.NE.0)  THEN
          IF (IUCOMP(IQ(KQS+L+1),IDV(2),NID).NE.0)   GO TO 21
          CALL MZDROP (IXSTJZ,L, '.')
          GO TO 21
        ENDIF

#if defined(CERNLIB_QDEBUG)
      IF (IFL.EQ.0)  THEN
          IFL = 1
          L   = LQJZ - 2
          GO TO 21
        ENDIF
#endif
      RETURN
      END
