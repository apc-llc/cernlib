*
* $Id: tcc1.F,v 1.1.1.1 1996/03/06 10:47:02 mclareni Exp $
*
* $Log: tcc1.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:02  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE TCC1

C-    Test processor level 2

      CHARACTER    CHNAME*4
      PARAMETER   (CHNAME = 'TCC1')

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

      DATA  MUSTAN / 5, 31, 32, 33, 34, 35, 14*0 /
      DATA  MUSTFL / 311, 312, 313, 314, 315, 5*0 /


      CALL JZIN (CHNAME, 0, 5, 0)

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
