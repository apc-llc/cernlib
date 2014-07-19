*
* $Id: fzotxt.F,v 1.2 1999/06/18 13:29:43 couet Exp $
*
* $Log: fzotxt.F,v $
* Revision 1.2  1999/06/18 13:29:43  couet
* - qcardl.inc was empty: It is now removed and not used.
*   Comment END CDE removed.
*
* Revision 1.1.1.1  1996/03/06 10:47:13  mclareni
* Zebra
*
*
#include "zebra/pilot.h"
      SUBROUTINE FZOTXT

C-    Ready the text vector for output

#include "zebra/zebq.inc"
#include "zebra/mzca.inc"
#include "zebra/fzcx.inc"
*
      EQUIVALENCE (LTX,LTEXTX)


      IF (LTX.EQ.0)                GO TO 49
      ND = IQ(KQSP+LTX+1)
      IF (ND.LT.2)                 GO TO 49
      NWTXX = ND
      IF (IFIFOX.NE.0)             GO TO 49
      IF (NWTXX.LE.MAXREX)         GO TO 49

C--       Text vector too large, truncate

      NWTXX = 0
      L  = LTX + 5
   24 N  = IQ(KQSP+L)
      IF (NWTXX+N+1.GT.MAXREX)     GO TO 49
      NWTXX = NWTXX + N + 1
      L     = L     + N + 1
      GO TO 24

   49 RETURN
      END
