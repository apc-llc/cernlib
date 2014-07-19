*
* $Id: zhytoz.F,v 1.1.1.1 1996/03/06 10:47:21 mclareni Exp $
*
* $Log: zhytoz.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:21  mclareni
* Zebra
*
*
#include "sys/CERNLIB_machine.h"
#include "_zebra/pilot.h"
      SUBROUTINE ZHYTOZ(IXDIV,IOFLAG,LSTR,*)
#include "bankparq.inc"
#include "mqsys.inc"
#include "qequ.inc"
#include "zstate.inc"
#include "zunit.inc"
      CHARACTER CHROUT*(*)
      PARAMETER (CHROUT = 'ZHYTOZ')

      CALL MZSDIV(IXDIV,1)

      IF (JQDIVI.LE.1.OR.JQDIVI.GT.20)                     GO TO 998

      CALL QTOZEB(0,0,0)

      IF (IQUEST(1).NE.0)  THEN
          PRINT '('' QTOZEB PROBLEMS IN HYDRA '')'
                                                           GO TO 998
      ENDIF

      NHYDRA = IQUEST(2)


      CALL MZRESV

      NQRESV = NQRESV - NHYDRA

      IF (NQRESV.LT.0) CALL MZGAR1

      MODEDV = IQMODE(KQT+JQDIVI)

      IF (MODEDV.EQ.0)  THEN
          LBEGIN = LQEND(KQT+JQDIVI)
      ELSE
          LBEGIN = LQSTA(KQT+JQDIVI)-NHYDRA
      ENDIF

      CALL QTOZEB(LQ(KQS+LBEGIN),LBEGIN,1)

#if defined(CERNLIB_QDEVZE)
      IF (NQDEVZ.NE.0) WRITE(IQLOG,'(100(6(2X,I7,''/'',I10),/))',ERR=11)
     +                       (L,LQ(KQS+L),L=LBEGIN,LBEGIN+NHYDRA-1)
   11 CONTINUE
#endif

      IF (IQUEST(1).NE.0)  THEN
          PRINT '('' QTOZEB PROBLEMS IN COPY  '')'
                                                           GO TO 998
      ENDIF

      LSTR   = IQUEST(2)
      IFORM  = IQUEST(3)

      CALL FZHYLN(LBEGIN,NHYDRA,IOFLAG,*998)

      CALL FZHYLS(LSTR,*998)


      KLSTR            = LOCF(LSTR) - LQSTOR
      LQLORG (KQS+LSTR) = KLSTR

      IF (MODEDV.EQ.0)  THEN
          LQEND(KQT+JQDIVI) = LQEND(KQT+JQDIVI)+NHYDRA
      ELSE
          LQSTA(KQT+JQDIVI) = LBEGIN
      ENDIF

#if defined(CERNLIB_QDEVZE)
      IF(NQDEVZ.NE.0) WRITE(IQLOG,'(100(6(2X,I7,''/'',I10),/))',ERR=12)
     +                      (L,LQ(KQS+L),L=LBEGIN,LBEGIN+NHYDRA-1)
   12 CONTINUE
#endif
      CALL QTOZEB(0,0,-1)
                                                           GO TO 999

  998 CALL QTOZEB(0,0,-1)
      RETURN 1

  999 RETURN
      END
