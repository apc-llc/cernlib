*
* $Id: fqcput.F,v 1.1.1.1 1996/03/06 10:47:01 mclareni Exp $
*
* $Log: fqcput.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:01  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE FQCPUT (MEMBUF,ICODE)

C-    Write to file for channel mode

#include "zebra/zunit.inc"
#include "zebra/quest.inc"
#include "test_include/fqtlun.inc"
C--------------    END CDE                             --------------
      DIMENSION    MEMBUF(999)

#include "zebra/q_jbit.inc"

#if defined(CERNLIB_QDEBPRI)
      IF (ICODE.NE.1)  CALL ZFATAM ('FQCPUT - ICODE not 1')
      IF (LOGLEV.GE.1)
     +       WRITE (IQLOG,9821) JMAKE,IQUEST(2),IQUEST(3)
      IF (LOGLEV.GE.3)  THEN
          IF (JBIT(IQUEST(3),1).NE.0)
     +       WRITE (IQLOG,9822) (MEMBUF(J),J=1,10)
        ENDIF
 9821 FORMAT (' FTESTC-  JMAKE=',I4,
     F', FQCPUT called with IQUEST(2),(3)=',I4,I6)
#endif
#if (defined(CERNLIB_QDEBPRI))&&(!defined(CERNLIB_B60M))
 9822 FORMAT (10X,'Header=',5Z13/17X,5Z13)
#endif
#if (defined(CERNLIB_QDEBPRI))&&(defined(CERNLIB_B60M))
 9822 FORMAT (10X,'Header=',5Z17/17X,5Z17)
#endif
      CALL XOUTBS (LUNT1,IQUEST(3),1,MEMBUF,IQUEST(2))
      IQUEST(1) = 0
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
