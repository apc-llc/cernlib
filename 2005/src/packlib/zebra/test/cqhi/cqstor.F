*
* $Id: cqstor.F,v 1.1.1.1 1996/03/06 10:47:00 mclareni Exp $
*
* $Log: cqstor.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:00  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE CQSTOR (JSTOFL)

C-    Create Zebra store for the test-run
C-    JSTOFL = 0 : normally, just create it
C-            -1 : get rid of the store with the highest number
C-                 and create it again from scratch

#include "zebra/mzbits.inc"
#include "zebra/quest.inc"
#include "zebra/mzca.inc"
#include "test_include/testla.inc"
#include "test_include/testll.inc"
#include "test_include/testdd.inc"
#include "test_include/testiq.inc"
C--------------    End CDE                             --------------


C----              Initialize the test store

      IF (JSTOFL.NE.0)  NQSTOR = NQSTOR - 1
      IF (NQSTOR.LT.0)  THEN
          NDATA = 19997
        ELSE
          NDATA = 20057
        ENDIF

      CALL MZSTOR (IXSTOR,'//', '.', FENCE
     +,            LQ,LA1,LX1,LQ(6000),IQ(NDATA))
      CALL CQFIXU (1,17000)

      CALL MZLOGL (IXSTOR,1)

      CALL MZWORK (IXSTOR,M,DLAST,0)
      CALL VZERO  (LQUSER,150)

      CALL MZLINK (IXSTOR,'/LAREX/', LAREX, LAREX(12),LAREX)
      CALL MZLINK (IXSTOR,'/LAREY/', LAREY, LAREY,    LAREY(12))
      CALL MZLINT (IXSTOR,'/LAREZ/',IFLREZ, LAREZ(7), LAREZ(12))
      RETURN
      END
