*
* $Id: fqmput.F,v 1.1.1.1 1996/03/06 10:47:01 mclareni Exp $
*
* $Log: fqmput.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:01  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE FQMPUT

C-    Write to file for memory mode

#include "zebra/zunit.inc"
#include "zebra/quest.inc"
#include "test_include/fqtlun.inc"
#include "fqmbuf.inc"
C--------------    END CDE                             --------------


#if defined(CERNLIB_QDEBPRI)
      IF (LOGLEV.GE.1)
     +       WRITE (IQLOG,9821) JMAKE,IQUEST(1),IQUEST(9)
      IF (LOGLEV.GE.3)
     +       WRITE (IQLOG,9822) (MEMBUF(J),J=1,10)
 9821 FORMAT (' FTESTM-  JMAKE=',I4,
     F', FZOUT returns IQUEST(1),(9)=',I4,I6)
#endif
#if (defined(CERNLIB_QDEBPRI))&&(!defined(CERNLIB_B60M))
 9822 FORMAT (10X,'Header=',5Z13/17X,5Z13)
#endif
#if (defined(CERNLIB_QDEBPRI))&&(defined(CERNLIB_B60M))
 9822 FORMAT (10X,'Header=',5Z17/17X,5Z17)
#endif
      IF (IQUEST(1).NE.0)    RETURN
      CALL XOUTB (LUNT1,MEMBUF,IQUEST(9))
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
