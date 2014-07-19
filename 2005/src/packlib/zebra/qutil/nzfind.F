*
* $Id: nzfind.F,v 1.3 1999/06/18 13:31:22 couet Exp $
*
* $Log: nzfind.F,v $
* Revision 1.3  1999/06/18 13:31:22  couet
* - qcardl.inc was empty: It is now removed and not used.
*   Comment END CDE removed.
*
* Revision 1.2  1996/04/18 16:13:40  mclareni
* Incorporate changes from J.Zoll for version 3.77
*
* Revision 1.1.1.1  1996/03/06 10:47:15  mclareni
* Zebra
*
*
#include "zebra/pilot.h"
      FUNCTION NZFIND (IXSTOR,LGO,ITEXT,JWORD)

C-    FIND AND COUNT ALL BANKS CONTAINING ITEXT IN WORD JWORD
C-    SEARCH LINEAR STRUCTURE STARTING WITH BANK AT LGO
C-    RETURN COUNT AS VALUE, BANK ADRS IN IQUEST

#include "zebra/q.inc"
#include "zebra/quest.inc"
*

#include "zebra/q_jbyt.inc"

#include "zebra/qstore.inc"

      NCOUNT = 0
      L      = LGO
      IF (L.EQ.0)                    GO TO 39
      IF (IQ(KQS+L+JWORD).EQ.ITEXT)   GO TO 24

   21 L = LQ(KQS+L)
      IF (L.EQ.0)                    GO TO 39
      IF (IQ(KQS+L+JWORD).NE.ITEXT)   GO TO 21

   24 NCOUNT = NCOUNT + 1
      IF (NCOUNT.GE.101)           GO TO 21
      IQUEST(NCOUNT) = L
      GO TO 21

   39 NZFIND = NCOUNT
      RETURN
      END
