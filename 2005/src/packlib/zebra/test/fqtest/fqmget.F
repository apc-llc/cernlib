*
* $Id: fqmget.F,v 1.1.1.1 1996/03/06 10:47:01 mclareni Exp $
*
* $Log: fqmget.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:01  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE FQMGET

C-    Read from file for memory mode

#include "zebra/zunit.inc"
#include "zebra/quest.inc"
#include "test_include/fqtlun.inc"
#include "fqmbuf.inc"
C--------------    END CDE                             --------------


      NWR  = NMEMOR
      CALL XINB (LUNT1,MEMBUF,NWR)
      IF (NWR.LE.0)                GO TO 94
      IQUEST(1) = 0
      RETURN

   94 IF (NWR.EQ.0)  THEN
          IQUEST(1) = 5
        ELSE
          IQUEST(1) = -5
        ENDIF
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
