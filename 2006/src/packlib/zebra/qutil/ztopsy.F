*
* $Id: ztopsy.F,v 1.3 1999/06/18 13:31:26 couet Exp $
*
* $Log: ztopsy.F,v $
* Revision 1.3  1999/06/18 13:31:26  couet
* - qcardl.inc was empty: It is now removed and not used.
*   Comment END CDE removed.
*
* Revision 1.2  1996/04/18 16:13:49  mclareni
* Incorporate changes from J.Zoll for version 3.77
*
* Revision 1.1.1.1  1996/03/06 10:47:15  mclareni
* Zebra
*
*
#include "zebra/pilot.h"
      SUBROUTINE ZTOPSY (IXSTOR,LGOP)

C-    Invert order of banks in a linear stucture

#include "zebra/q.inc"
*
      DIMENSION    LGOP(9)

#include "zebra/q_jbyt.inc"

      LN = LGOP(1)
      IF (LN.EQ.0)           RETURN

#include "zebra/qstore.inc"

      KGO = LQ(KQS+LN+2)
      LC  = 0

   21 LP = LC
      LC = LN
      LN = LQ(KQS+LN)
      LQ(KQS+LC)   = LP
      LQ(KQS+LC+2) = LN
      IF (LN.NE.0)                 GO TO 21

      IF (KGO.NE.0)  LQ(KQS+KGO) = LC
      LQ(KQS+LC+2) = KGO
      LGOP(1)   = LC
      RETURN
      END
