*
* $Id: fqtinf.F,v 1.1.1.1 1996/03/06 10:47:02 mclareni Exp $
*
* $Log: fqtinf.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:02  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE FQTINF (LUN)

C-    Call FZINFO for LUN, if not zero
C-    Dump the FZSTAT information
C-    FQTINF operates only if the IFLXQ(1) flag is set

#include "zebra/zunit.inc"
#include "test_include/testll.inc"
#include "test_include/testdd.inc"
#include "test_include/testiq.inc"
#include "test_include/testee.inc"
#include "zebra/fzstat.inc"
C--------------    END CDE                             --------------


      IF (IFLXQ(1).EQ.0)           RETURN

      WRITE (IQPRNT,9001) LUN
 9001 FORMAT (1X/' FQTINF',I3,' :'/)

C--       Call FZINFO only if LUN is not zero
C-                 for dumping the status as left by FZENDO

      IF (LUN.NE.0)  CALL FZINFO (LUN)

      WRITE (IQPRNT,9011) INFLUN
 9011 FORMAT (10X,'INFLUN =',I4)

      IF (INFLUN.EQ.0)             RETURN

      WRITE (IQPRNT,9012) INFSTA
 9012 FORMAT (10X,'INFSTA =',Z17,' hex')

      WRITE (IQPRNT,9013)
 9013 FORMAT (10X,'INFOFZ =')

      WRITE (IQPRNT,9014) (INFOFZ(J),J,J=1,24)
 9014 FORMAT ((10X,5(I10,I3)))

      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
