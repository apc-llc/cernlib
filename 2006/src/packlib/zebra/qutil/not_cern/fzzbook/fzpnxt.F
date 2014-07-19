*
* $Id: fzpnxt.F,v 1.1.1.1 1996/03/06 10:47:21 mclareni Exp $
*
* $Log: fzpnxt.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:21  mclareni
* Zebra
*
*
* ----------------------------------------------------------
#include "sys/CERNLIB_machine.h"
#include "_zebra/pilot.h"
      SUBROUTINE FZPNXT (IXSTOR,LUN,IDCAT,IERR)
#include "dzc1.inc"
#include "mqsys.inc"
#include "qequ.inc"
#include "mzcn.inc"
#include "bankparq.inc"

      INTEGER IH(4)
      CHARACTER CHROUT*(*)
      PARAMETER (CHROUT = 'FZPNXT')

#include "q_jbit.inc"

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
      IBUF   = IUCOMP (LUN,IQ(KQS+LBUF+1),NUNIT)
      IF (IBUF.EQ.0)                     THEN
          IERR = 900
                                                           GO TO 999
      ELSE
          LBUF   = LQ(KQS+LBUF-IBUF)
          CALL MZCHLS(NCHEKQ,LBUF)
          IF (IQFOUL.NE.0)                   THEN
              IERR = 1000*IQFOUL
                                                           GO TO 999
         ENDIF
      ENDIF


  100 CALL EPFHDR (LUN,4,IH,IQ(KQS+LBUF+3),IERR)
      IF (IERR.NE.0.AND.IERR.NE.6)                         GO TO 999
      IF (IH(2).NE.1)                                      GO TO 100

      IDCAT    = IH(4)
      LF       = KQS+LBUF+1
      IQ(LF)   = 1
      IQ(LF+1) = IDCAT

  998 CONTINUE
  999 RETURN
      END
