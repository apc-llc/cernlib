*
* $Id: cqhids.F,v 1.1.1.1 1996/03/06 10:47:00 mclareni Exp $
*
* $Log: cqhids.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:00  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE CQHIDS (JMAKE,JFILL)

C-    Create complex high level test structure
C-    by repeated calls to CQLODS

#include "test_include/testll.inc"
#include "test_include/testdd.inc"
#include "test_include/testiq.inc"
#include "test_include/testee.inc"
C--------------    End CDE                             --------------


      NINT  = JMAKE
      M(10) = NINT
      NCODE = NINT / 4
      NSTR  = NINT - 4*NCODE + 1

      CALL UPKBYT (NCODE,1,M(11),6,0)
      LQMAIN = 0
      LQUSER(7) = 0

      DO 69 JSTR=1,NSTR

C--                Set a dead area

      IF (NSTR.NE.4)               GO TO 32
      IF (JSTR.NE.3)               GO TO 32
      IF (M(12)+M(13).NE.0)        GO TO 32
      CALL ZSHUNT (IXSTOR,LQMAIN,LQUSER(7),1,1)
      CALL CQLODS (0,-1)
      CALL MZDROP (IXSTOR,LQMAIN,'L')
      CALL ZSHUNT (IXSTOR,LQUSER(7),LQMAIN,1,1)
   32 CONTINUE

C--                Create new set of banks

      JDIV = 2
      IF (JSTR.EQ.NSTR)  JDIV=0
      CALL CQLODS (JDIV,JFILL)

C--                Drop some banks according to code jmake

      DO 39 J=1,6
      IF (M(J+10).EQ.0)            GO TO 39
      IF (JDIV.EQ.0)  CALL MZDROP (IXSTOR,LLV(J),'.')
                      CALL MZDROP (IXSTOR,LAV(J),'.')
   39 CONTINUE
   69 CONTINUE
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
