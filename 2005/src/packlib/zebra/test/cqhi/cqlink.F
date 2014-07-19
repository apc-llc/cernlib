*
* $Id: cqlink.F,v 1.1.1.1 1996/03/06 10:47:00 mclareni Exp $
*
* $Log: cqlink.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:00  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE CQLINK (NOCHK,LINKV,NL)

C-    Dump vector of links whith names of pointed banks
C-    Requires format 'LLLLLLL' set to CQHEAD

#include "test_include/cqc.inc"
#include "zebra/zbcd.inc"
#include "zebra/mqsys.inc"
#include "zebra/mzcn.inc"
C--------------    End CDE                             --------------
      DIMENSION    LINKV(7), IV(14)


      IF (NOCHK.LT.0)  IQCOPT(8)=-7

      N = 0
      J = 1

   21 LINK = LINKV(J)

      IV(N+1) = LINK
      IF (LINK.EQ.0)               GO TO 24
      CALL MZCHLS (-7,LINK)
      IF (IQFOUL.NE.0)             GO TO 24
      IV(N+2) = IQ(KQS+LINK-4)
      GO TO 28

   24 IV(N+2) = IQBLAN
   28 N = N + 2
      J = J + 1
      IF (J.GE.8)                  GO TO 39
      IF (J.LE.NL)                 GO TO 21

   39 CALL CQDATA (IV(1),IV(N))
      IF (NOCHK.NE.0)  IQCOPT(8) = 0
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
