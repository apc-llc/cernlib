*
* $Id: fzhyls.F,v 1.1.1.1 1996/03/06 10:47:21 mclareni Exp $
*
* $Log: fzhyls.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:21  mclareni
* Zebra
*
*
#include "sys/CERNLIB_machine.h"
#include "_zebra/pilot.h"
      SUBROUTINE FZHYLS (LHEAD,*)
#include "bankparq.inc"
#include "dzc1.inc"
#include "zmach.inc"
#include "zstate.inc"
#include "zunit.inc"
#include "mqsys.inc"
#include "mzcn.inc"
#include "qequ.inc"
      CHARACTER CHROUT*(*)
      PARAMETER (CHROUT = 'FZHYLS')

#include "q_jbit.inc"
      IF (LHEAD.EQ.0)                                      GO TO 999

      LEV       = NQOFFS(1) + LQEND(1) + 3
      LEVE      = LEV + 1590
      LQ(LEV-2) = 0
      LQ(LEV-1) = 0
      LQ(LEV)   = LHEAD

      LCUR      = LHEAD
      LX        = LHEAD

      IQWIDN(KQS+LHEAD) = 0
      LQLUP(KQS+LHEAD)  = 0
      LQLORG(KQS+LHEAD) = 0


   20 LAST = LCUR - IQWNS(KQS+LCUR)


   24 IF (LX.LT.LAST)                                      GO TO 41
      LNEW = LQ(KQS+LX)
      LX   = LX - 1
      IF (LNEW.EQ.0)                                       GO TO 24

      IF (LQLUP(KQS+LNEW).NE.IQNIL)                        GO TO 24


      LQ(LEV+1) = LX
      LQ(LEV+2) = LCUR

      LEV = LEV + 3

      IF (LEV.GT.LEVE)                 THEN
          WRITE(IQPRNT,
     W   '(''0FZHYLS -- ????? '',T40,'' Stack overflow'')')
          NQUSED = NQUSED + 2
                                                           GO TO 998
      ENDIF

      LQ(LEV)   = LNEW

      IF (LCUR.EQ.LX+1) THEN
          IQWIDN(KQS+LNEW) = IQWIDN(KQS+LCUR) + 1
          LQLUP(KQS+LNEW)  = LQLUP(KQS+LCUR)
      ELSE
          IQWIDN(KQS+LNEW) = LCUR - LX - 1
          LQLUP(KQS+LNEW)  = LCUR
      ENDIF
      LQLORG(KQS+LNEW) = LX+1
#if defined(CERNLIB_QDEVZE)
      IF (NQDEVZ.NE.0)
     +PRINT 9831, LEV,LCUR,LX+1,LNEW
 9831 FORMAT (' DEVZE FZHYLS,  DOWN.   LEV,LCUR,LX+1,LNEW=',6I8)
#endif


   32 LCUR = LNEW
      LNEW = LQ(KQS+LCUR)
      IF (LNEW.EQ.0)                                       GO TO 36
#if defined(CERNLIB_QDEVZE)
      IF (NQDEVZ.NE.0)
     +PRINT 9833, LCUR,LNEW
 9833 FORMAT (' DEVZE FZHYLS,  ALONG.  LCUR,LNEW=',6I8)
#endif
      IF (LQLUP(KQS+LNEW).NE.IQNIL)                        GO TO 36

      IQWIDN(KQS+LNEW) = IQWIDN(KQS+LCUR) + 1
      LQLUP(KQS+LNEW)  = LQLUP(KQS+LCUR)
      LQLORG(KQS+LNEW) = LCUR
                                                           GO TO 32

   36 CONTINUE
      LAST = LCUR - IQWNS(KQS+LCUR)
      LX   = LCUR - 1
                                                           GO TO 24


   41 LNEW = LCUR
      IF (LCUR.EQ.LQ(LEV))                                 GO TO 46
      LCUR = LQLORG(KQS+LCUR)
      LX   = LCUR - 1
#if defined(CERNLIB_QDEVZE)
      IF (NQDEVZ.NE.0)
     +PRINT 9841, LCUR,LNEW
 9841 FORMAT (' DEVZE FZHYLS,  BACK.   LCUR,LNEW=',6I8)
#endif
                                                           GO TO 20


   46 LEV  = LEV - 3
      LX   = LQ(LEV+1)
      LCUR = LQ(LEV+2)
#if defined(CERNLIB_QDEVZE)
      IF (NQDEVZ.NE.0)
     +PRINT 9846, LEV,LCUR,LX
 9846 FORMAT (' DEVZE FZHYLS,  UP.     LEV,LCUR,LX=',6I8)
#endif
      IF (LCUR.NE.0)                                       GO TO 20


                                                           GO TO 999
  998 RETURN 1
  999 RETURN
      END
