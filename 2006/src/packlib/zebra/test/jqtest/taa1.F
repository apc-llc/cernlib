*
* $Id: taa1.F,v 1.1.1.1 1996/03/06 10:47:02 mclareni Exp $
*
* $Log: taa1.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:02  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE TAA1

C-    Test processor level 2

      CHARACTER    CHNAME*4
      PARAMETER   (CHNAME = 'TAA1')

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

      DATA  MUSTAN / 3, 11, 12, 13, 16*0 /
      DATA  MUSTFL / 111, 112, 113, 114, 115, 5*0 /


      CALL JZIN (CHNAME, 0, 3, 0)

      CALL MZWORK (IXSTOR, LIWSXX,MWSP(24), 0)

      CALL TJMUAN (CHNAME,MUSTAN)
      CALL TJMUFL (CHNAME,MUSTFL)

      NENT = IQ(LQSV+2)
      CALL JZTELL (NENT/4)

      CALL TJIDLE (4)

      CALL JZOUT (CHNAME)
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
