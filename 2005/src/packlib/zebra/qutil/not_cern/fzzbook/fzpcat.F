*
* $Id: fzpcat.F,v 1.1.1.1 1996/03/06 10:47:21 mclareni Exp $
*
* $Log: fzpcat.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:21  mclareni
* Zebra
*
*
* ----------------------------------------------------------
#include "sys/CERNLIB_machine.h"
#include "_zebra/pilot.h"
      SUBROUTINE FZPCAT (IXDIV,LUN,LCAT,IDCAT,IERR)
#include "dzc1.inc"
#include "mqsys.inc"
#include "qequ.inc"
#include "mzcn.inc"
#include "bankparq.inc"
#include "bkfoparq.inc"

      CHARACTER CHROUT*(*)
      PARAMETER (CHROUT = 'FZPCAT')

#include "q_jbit.inc"

#if (defined(CERNLIB_DEBUGON))&&(defined(CERNLIB_VFORT))
#include "debugvf2.inc"
#endif
      IERR   = 0

      CALL MZSDIV(IXDIV,NCHEKQ)
      LBUF   = LQSYSS(KQT+MSYSPQ)
      IF (LBUF.NE.0)                             THEN

          CALL MZCHLS(NCHEKQ,LBUF)
          IF (IQFOUL.NE.0)                   THEN
              IERR = 1000*IQFOUL
                                                           GO TO 999
          ENDIF
          NUNIT  = IQWND(KQS+LBUF)
          IBUF   = IUCOMP (LUN,IQ(KQS+LBUF+1),NUNIT)
          IF (IBUF.NE.0)                     THEN
              LBUF   = LQ(KQS+LBUF-IBUF)+1
              IF (IQ(KQS+LBUF).EQ.1)     THEN
                  IDCAT  = IQ(KQS+LBUF+1)
                                                           GO TO 100
              ENDIF
          ENDIF
      ENDIF


      CALL FZPNXT (IXDIV,LUN,IDCAT,IERR)
      IF (IERR.NE.0.AND.IERR.NE.6)                         GO TO 999


  100 CALL EPGETW (LUN,20,IW20,IERR)
      IF (IERR.NE.0)                                       GO TO 999

      CALL EPGETW (LUN,21,IW21,IERR)
      IF (IERR.NE.0)                                       GO TO 999

      NKEY   = (IW20 - IW21) / 2


      CALL MZCHLS(NCHEKQ,LCAT)
      IF (IQFOUL.EQ.0)                   THEN
          NPUSH  = NKEY - IQWND(KQS+LCAT)
          CALL MZPUSH(IXDIV,LCAT,0,NPUSH,'I')
      ELSE
          CALL MZBOOK(IXDIV,LCAT,LCAT,1,'*CAT',0,0,NKEY,IFOINQ,0)
      ENDIF


      LBUF   = LQSYSS(KQT+MSYSPQ)
      NUNIT  = IQWND(KQS+LBUF)
      IBUF   = IUCOMP (LUN,IQ(KQS+LBUF+1),NUNIT)
      LBUF   = LQ(KQS+LBUF-IBUF)+3


      CALL EPFRD (LUN,13,NW,IQ(KQS+LCAT+1),IQ(KQS+LBUF),IERR)
      IF (IERR.NE.0)                                       GO TO 999
      LF         = LBUF - 2
      IQ(KQS+LF) = 2
      CALL ZFRIBM (IQ(KQS+LCAT+1),NKEY,2)

  998 CONTINUE
  999 RETURN
      END
