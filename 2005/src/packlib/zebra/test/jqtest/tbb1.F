*
* $Id: tbb1.F,v 1.1.1.1 1996/03/06 10:47:02 mclareni Exp $
*
* $Log: tbb1.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:02  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE TBB1

C-    Test processor level 2

      CHARACTER    CHNAME*4
      PARAMETER   (CHNAME = 'TBB1')

#include "zebra/zunit.inc"
#include "zebra/quest.inc"
#include "test_include/testla.inc"
#include "test_include/testll.inc"
#include "tejqdd.inc"
#include "test_include/testiq.inc"
#include "test_include/testee.inc"
#include "zebra/jzuc.inc"
C--------------    End CDE                             --------------
      DIMENSION    MUSTAN(20), MUSTFL(10)

      DATA  MUSTAN / 4, 21, 22, 23, 24, 15*0 /
      DATA  MUSTFL / 211, 212, 213, 214, 215, 5*0 /


      CALL JZIN (CHNAME, 0, 4, 0)

      CALL MZWORK (IXSTOR, LIWSXX,MWSP(24), 0)

      CALL TJMUAN (CHNAME,MUSTAN)
      CALL TJMUFL (CHNAME,MUSTFL)

      NENT = IQ(LQSV+2)
      CALL JZTELL (NENT/4)

      CALL TJIDLE (1)

      IF (NENT.EQ.124)  CALL JZTRAC (0)

      CALL JZOUT (CHNAME)
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
