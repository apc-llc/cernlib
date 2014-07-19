*
* $Id: mzfdiv.F,v 1.3 1999/06/18 13:30:43 couet Exp $
*
* $Log: mzfdiv.F,v $
* Revision 1.3  1999/06/18 13:30:43  couet
* - qcardl.inc was empty: It is now removed and not used.
*   Comment END CDE removed.
*
* Revision 1.2  1996/04/18 16:13:01  mclareni
* Incorporate changes from J.Zoll for version 3.77
*
* Revision 1.1.1.1  1996/03/06 10:47:22  mclareni
* Zebra
*
*
#include "zebra/pilot.h"
      FUNCTION MZFDIV (IXST,LIXP)

C-    Find the division into which LIX points, in the current store;
C-    return its number,  not pointing into any : return zero

#include "zebra/mqsys.inc"
*
      DIMENSION    IXST(9), LIXP(9)

#include "zebra/q_jbyt.inc"

      IXSTOR = IXST(1)
      LIX    = LIXP(1)
      IF (IXSTOR.NE.-7)  THEN
#include "zebra/qstore.inc"
          JDIVI = 2
        ELSE
          JDIVI = JQDIVI
        ENDIF

      IF (JDIVI.EQ.0)              GO TO 21

      IF (LIX.LT.LQSTA(KQT+JDIVI))    GO TO 21
      IF (LIX.LT.LQEND(KQT+JDIVI))    GO TO 99

C--                Find division

   21 JDIVI = 1
      IF (LIX.LT.LQEND(KQT+JQDVLL))   GO TO 24
      IF (LIX.GE.LQEND(KQT+20))       GO TO 91
      JDIVI = JQDVSY

   24 IF (LIX.LT.LQEND(KQT+JDIVI))    GO TO 26
      JDIVI = JDIVI + 1
      GO TO 24

   26 IF (LIX.GE.LQSTA(KQT+JDIVI))    GO TO 99

   91 JDIVI = 0
   99 MZFDIV = JDIVI
      RETURN
      END
