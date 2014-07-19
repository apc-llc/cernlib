*
* $Id: fzpend.F,v 1.1.1.1 1996/03/06 10:47:21 mclareni Exp $
*
* $Log: fzpend.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:21  mclareni
* Zebra
*
*
* ----------------------------------------------------------
#include "sys/CERNLIB_machine.h"
#include "_zebra/pilot.h"
      SUBROUTINE FZPEND(IXSTOR,LUN,IERR)
#include "dzc1.inc"
#include "mqsys.inc"
#include "qequ.inc"
#include "mzcn.inc"
#include "bankparq.inc"

      CHARACTER CHROUT*(*)
      PARAMETER (CHROUT = 'FZPEND')

#if (defined(CERNLIB_DEBUGON))&&(defined(CERNLIB_VFORT))
#include "debugvf2.inc"
#endif
      IERR   = 0

      CALL MZSDIV(IXSTOR,NCHEKQ)


      LBUF   = LQSYSS(KQT+MSYSPQ)

      CALL MZCHLS(NCHEKQ,LBUF)
      IF (IQFOUL.NE.0)                   THEN
          IERR = 1000*IQFOUL
                                                           GO TO 999
      ENDIF

      NUNIT  = IQWND(KQS+LBUF)

      DO 100 ILUN = 1,NUNIT
          IF (LUN.EQ.0.OR.LUN.EQ.IQ(KQS+LBUF+ILUN)) THEN
              CALL MZDROP(IXSTOR,LQ(KQS+LBUF-ILUN),' ')
              IQ(KQS+LBUF+ILUN) = 0
          ENDIF
  100 CONTINUE

  998 CONTINUE
  999 RETURN
      END
