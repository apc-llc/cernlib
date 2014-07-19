*
* $Id: cqfixu.F,v 1.1.1.1 1996/03/06 10:47:00 mclareni Exp $
*
* $Log: cqfixu.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:00  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE CQFIXU (MSWIT, IPARAP)

C-    Fix-up operations for the standard test run

#include "test_include/cqc.inc"
#include "zebra/mqsys.inc"
C--------------    End CDE                             --------------
      DIMENSION    IPARAP(12)

#include "zebra/q_jbit.inc"


      IPARA = IPARAP(1)

      GO TO (11,21,31), MSWIT

C--                Place division 2 to fixed adr

   11 LQEND(KQT+2) = IPARA
      LQSTA(KQT+2) = LQEND(KQT+2)
      RETURN

C--                Calc. gap available to division

   21 JDV  = IPARA
      MODE = JBIT (IQMODE(KQT+JDV),1)
      IF (MODE.NE.0)               GO TO 24
      JNX = JDV + 1
      IF (JDV.EQ.JQDVLL)  JNX=JQDVSY
      GO TO 27

   24 JNX = JDV
      JDV = JDV - 1
      IF (JNX.EQ.JQDVSY)  JDV=JQDVLL
   27 IPARAP(1) = LQSTA(KQT+JNX) - LQEND(KQT+JDV)
      RETURN

   31 CONTINUE
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
