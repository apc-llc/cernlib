*
* $Id: fzpdef.F,v 1.1.1.1 1996/03/06 10:47:21 mclareni Exp $
*
* $Log: fzpdef.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:21  mclareni
* Zebra
*
*
* ----------------------------------------------------------
#include "sys/CERNLIB_machine.h"
#include "_zebra/pilot.h"
      SUBROUTINE FZPDEF(IXSTOR,LUN,NREC,IERR)
#include "dzc1.inc"
#include "mqsys.inc"
#include "qequ.inc"
#include "mzcn.inc"
#include "zmach.inc"
#include "bankparq.inc"
#include "bkfoparq.inc"

      CHARACTER CHROUT*(*)
      PARAMETER (CHROUT = 'FZPDEF')

#include "q_jbit.inc"

#if (defined(CERNLIB_DEBUGON))&&(defined(CERNLIB_VFORT))
#include "debugvf2.inc"
#endif
      IERR   = 0

      CALL MZSDIV(IXSTOR,NCHEKQ)



      LBUF   = LQSYSS(KQT+MSYSPQ)
      IF (LBUF.NE.0)                             THEN
          CALL MZCHLS(NCHEKQ,LBUF)
          IF (IQFOUL.NE.0)                   THEN
              IERR = 1000*IQFOUL
                                                           GO TO 999
          ENDIF
      ELSE
          CALL EPINIT
          CALL MZBOOK(IXSTOR+JQDVSY,LBUF,LQSYSS(KQT+MSYSPQ),1,
     +                '*FZP',1,1,1,IFOINQ,0                   )
      ENDIF

      NUNIT  = IQWND(KQS+LBUF)
      IBUF   = IUCOMP (LUN,IQ(KQS+LBUF+1),NUNIT)
      IF (IBUF.NE.0) THEN
          CALL MZDROP(IXSTOR,LBUF,' ')
          ILUN   = IBUF
      ELSE
          DO 100 ILUN=1,NUNIT
              IF (LQ(KQS+LBUF-ILUN).EQ.0)                  GO TO 200
  100     CONTINUE
          CALL MZPUSH(IXSTOR,LBUF,1,1,'I')
          ILUN   = NUNIT+1
      ENDIF


  200 NR     = NREC
      IF (NR.LE.0) NR = 1800
      NW     = (16 * NR - 1) / NQBITW + 3
      IF(NREC.GT.0) CALL EPSETW(LUN,1,LR,IERR)
      IF (IERR.NE.0)                                       GO TO 999
      CALL EPSETW(LUN,5,-1,IERR)
      IF (IERR.NE.0)                                       GO TO 999
      CALL EPSETW(LUN,6,-1,IERR)
      IF (IERR.NE.0)                                       GO TO 999
      IQ(KQS+LBUF+ILUN) = LUN

      CALL MZBOOK(IXSTOR+JQDVSY,LBUF,LBUF,-ILUN,'*LUN',0,0,NW,IFOBIQ,0)

  998 CONTINUE
  999 RETURN
      END
