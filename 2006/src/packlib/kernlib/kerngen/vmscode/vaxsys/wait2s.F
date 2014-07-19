*
* $Id: wait2s.F,v 1.1.1.1 1996/02/15 17:50:27 mclareni Exp $
*
* $Log: wait2s.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:27  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      SUBROUTINE WAIT_2SEC
C
C CERN PROGLIB# Z301    WAIT2S          .VERSION KERNVAX  2.04  830531
C ORIG. 11/05/83 JZ
C
C-    PAUSE FOR 2 SECONDS
C-    THIS ROUTINE USES EVENT FLAG 0

      INTEGER      DELTAT(2)
      INTEGER      SYS$SETIMR


C     TYPE 100
C 100 FORMAT ('0WAIT  2 SECS')
      CALL SYS$BINTIM ('0 ::02',DELTAT)
      ISTAT = SYS$SETIMR (%VAL(0),DELTAT,,)
      IF (.NOT.ISTAT)                        RETURN
      CALL SYS$WAITFR (%VAL(0))
      RETURN
      END
