*
* $Id: mziocf.F,v 1.2 1999/06/18 13:30:45 couet Exp $
*
* $Log: mziocf.F,v $
* Revision 1.2  1999/06/18 13:30:45  couet
* - qcardl.inc was empty: It is now removed and not used.
*   Comment END CDE removed.
*
* Revision 1.1.1.1  1996/03/06 10:47:21  mclareni
* Zebra
*
*
#include "zebra/pilot.h"
      SUBROUTINE MZIOCF (JUP,MXVAL)

C-    Find largest possible size of next group, slave to MZIOCH

#include "zebra/zkrakc.inc"
#include "zebra/quest.inc"
*
      DIMENSION    MU(99)
      EQUIVALENCE (MU(1),IQHOLK(1))

      EQUIVALENCE (NGR,IQUEST(1)), (NGRU,IQUEST(2))

      DIMENSION    JUP(9), MXVAL(9)


      JU  = JUP(1)
      MXC = MU(JU+2)

      DO 24  JL=2,NGR
      JU = JU + 2
      MXC = MAX (MU(JU+2),MXC)
      IF (MXC.GE.MXVAL(JL))        GO TO 29
   24 CONTINUE
      NGRU = NGR
      RETURN

   29 NGRU = JL - 1
      RETURN
      END
