*
* $Id: fzploc.F,v 1.1.1.1 1996/03/06 10:47:21 mclareni Exp $
*
* $Log: fzploc.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:21  mclareni
* Zebra
*
*
* ----------------------------------------------------------
#include "sys/CERNLIB_machine.h"
#include "_zebra/pilot.h"
      SUBROUTINE FZPLOC (IXSTOR,KEY,LOCID,NMEMBS,MODE)
#include "dzc1.inc"
#include "mqsys.inc"
#include "qequ.inc"
#include "mzcn.inc"
#include "bankparq.inc"

      CHARACTER CHROUT*(*)
      PARAMETER (CHROUT = 'FZPLOC')

#if (defined(CERNLIB_DEBUGON))&&(defined(CERNLIB_VFORT))
#include "debugvf2.inc"
#endif

      CALL MZSDIV(IXSTOR,0)

      LEPK   = LQSYSS(KQT+MSYSKQ)
      CALL MZCHLS(NCHEKQ,LEPK)
      IF (IQFOUL.NE.0)                   THEN
          IERR = 1000*IQFOUL
                                                           GO TO 999
      ENDIF

      LOCID  = 0
      LOCID  = 0

      LKEY   = LQ(KQS+LEPK-1)
      NKEY   = IQWND(KQS+LKEY)


      IKEY   = 0
      IUP    = NKEY + 1
      IDOWN  = 0

  10      IF (IUP-IDOWN.LE.1)                              GO TO 999
          IMEAN  = (IUP+IDOWN) / 2
          IKEY   = IMEAN
          IF (KEY-IQ(KQS+LKEY+IKEY)) 20,40,30
  20      IUP    = IMEAN
                                                           GO TO 10
  30      IDOWN  = IMEAN
          IKEY   = IKEY+ 1
                                                           GO TO 10
   40 LNID   = LQ(KQS+LEPK-2)
      LNEL   = LQ(KQS+LEPK-3)
      LCOD   = LQ(KQS+LEPK-4)
      LOCID  = IQ(KQS+LNID+IKEY)
      NMEMBS = IQ(KQS+LNEL+IKEY)
      MODE   = IQ(KQS+LCOD+IKEY)

  998 CONTINUE
  999 RETURN
      END
