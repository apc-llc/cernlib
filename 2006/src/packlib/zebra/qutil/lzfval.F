*
* $Id: lzfval.F,v 1.3 1999/06/18 13:31:21 couet Exp $
*
* $Log: lzfval.F,v $
* Revision 1.3  1999/06/18 13:31:21  couet
* - qcardl.inc was empty: It is now removed and not used.
*   Comment END CDE removed.
*
* Revision 1.2  1996/04/18 16:13:34  mclareni
* Incorporate changes from J.Zoll for version 3.77
*
* Revision 1.1.1.1  1996/03/06 10:47:14  mclareni
* Zebra
*
*
#include "zebra/pilot.h"
#if defined(CERNLIB_VAXVMS)
#include "qutyvax/lzfval.F"
#else
      FUNCTION LZFVAL (IXSTOR,LGO,VAL,TOL,JWORD)

C-    FIND BANK CONTAINING IN WORD JWORD FL.P. VAL WITHIN TOL
C-    SEARCH LINEAR STRUCTURE STARTING WITH BANK AT LGO
C-    RETURN BANK-ADR AS VALUE

#include "zebra/q.inc"
*

#include "zebra/q_jbyt.inc"

#include "zebra/qstore.inc"

      VLOW = VAL - TOL
      VHI  = VAL + TOL

      L = LGO
   21 IF (L.EQ.0)                    GO TO 29
      IF (Q(KQS+L+JWORD).LT.VLOW)     GO TO 22
      IF (Q(KQS+L+JWORD).LE.VHI)      GO TO 29
   22 L = LQ(KQS+L)
      GO TO 21

   29 LZFVAL= L
      RETURN
      END
#endif
