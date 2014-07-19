*
* $Id: tzmain.F,v 1.1.1.1 1996/03/06 10:47:05 mclareni Exp $
*
* $Log: tzmain.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:05  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      PROGRAM TZMAIN

#include "zebra/zstate.inc"
#undef CERNLIB_ZEBRA_ZSTATE_INC
#include "zebra/zunit.inc"
#undef CERNLIB_ZEBRA_ZUNIT_INC
#include "test_include/cqc.inc"
C--------------    End CDE                             --------------
#include "test_include/tpseudo.inc"
      DIMENSION    LISTIN(4)

#include "test_include/tzmaindc.inc"
* Ignoring t=pass

      DATA  LISTIN / 3, 1,-1, 0 /

#if defined(CERNLIB_QMIBM)||defined(CERNLIB_QMIBMD)
      OPEN (16,FILE='/ZXTEST DAT',STATUS='OLD')
#elif defined(CERNLIB_QMND3)
      OPEN (16,FILE='ZXTEST:DAT')
#elif defined(CERNLIB_QMPDP)

#else
      OPEN (16,FILE='zxtest.dat',STATUS='OLD')
#endif

      CALL MZEBRA (LISTIN)
      CALL MZVERS
      CALL CQBKIN


C------            Set CQ Options        --------
C--   Execution
      IQCOPT(1)= 7
C--   CQ Output (Comparison Failure forces output)
      IQCOPT(2)= 7
      IQCOPT(2)= 0
C--   Punch new Check Cards
      IQCOPT(3)= 7
      IQCOPT(3)= 0
C--   Compare against old Check Cards
      IQCOPT(4)= 0
      IQCOPT(4)= 7
C--   CALL QFATAL for Comparison Failure
      IQCOPT(5)= 7
      IQCOPT(5)= 0
C--   Maximum output
      IQCOPT(6)= 107
      IQCOPT(6)= 7
      IQCOPT(6)= 0
C--   Check machine dependent data
      IQCOPT(7)= 7
      IQCOPT(7)= 0


      CALL CQINIT (16)

      WRITE (IQLOG,9011)
 9011 FORMAT (1X/' *!!!!!!  Test of MZ memory management')

      CALL ZETEMQ

      CALL MZEND

      CALL MZEBRA (LISTIN)
      CALL MZVERS
#include "test_include/tpseudin.inc"
      CALL ZETEMQ

      WRITE (IQLOG,9098) NQCF
      IF (IQTYPE.NE.0)  THEN
          IF (IQTYPE.NE.IQLOG)  WRITE (IQTYPE,9098) NQCF
        ENDIF
      IF (NQCF.NE.0)  NQERR=MAX(NQERR,7)
      CALL ZEND

 9098 FORMAT (1X/' *!!!!--  End of test :',I5,' comparison failures')
       END
