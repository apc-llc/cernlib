*
* $Id: fqcget.F,v 1.2 1996/04/18 16:14:31 mclareni Exp $
*
* $Log: fqcget.F,v $
* Revision 1.2  1996/04/18 16:14:31  mclareni
* Incorporate changes from J.Zoll for version 3.77
*
* Revision 1.1.1.1  1996/03/06 10:47:01  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE FQCGET (MEMBUF,ICODE)

C-    Read from file for channel mode

#include "zebra/zunit.inc"
#include "zebra/quest.inc"
#include "test_include/fqtlun.inc"
C--------------    END CDE                             --------------
      DIMENSION    MEMBUF(999)


#if defined(CERNLIB_QDEBPRI)
      IF (ICODE.NE.0)  CALL ZFATAM ('FQCGET - ICODE not 0')
      IF (LOGLEV.GE.1)
     +       WRITE (IQLOG,9821) JMAKE,IQUEST(2),IQUEST(3)
 9821 FORMAT (' FTESTC-  JMAKE=',I4,
     F', FQCGET called with IQUEST(2),(3)=',I4,I6)
#endif

      NWRMX  = IQUEST(2)
      NWR    = NWRMX
      CALL XINBS (LUNT1,INFLAG,1,MEMBUF,NWR)
      IF (NWR.LE.0)                GO TO 94
      IQUEST(1) = 0
      IQUEST(2) = NWR
      RETURN

C----              Read error / End-of-file

   94 IF (NWR.EQ.0)                GO TO 98
      IQUEST(1) = -NWR
      RETURN

   98 IQUEST(1) = -1
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
