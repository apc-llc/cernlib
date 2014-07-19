*
* $Id: tccc.F,v 1.1.1.1 1996/03/06 10:47:02 mclareni Exp $
*
* $Log: tccc.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:02  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE TCCC

C-    Test processor level 1

      CHARACTER    CHNAME*4
      PARAMETER   (CHNAME = 'TCCC')

#include "zebra/zunit.inc"
#include "zebra/quest.inc"
#include "test_include/testla.inc"
#include "test_include/testll.inc"
#include "tejqdd.inc"
#include "test_include/testiq.inc"
#include "test_include/testee.inc"
#include "zebra/jzuc.inc"
C--------------    End CDE                             --------------
      DIMENSION    MUSTAN(20), MUSTFL(10), LISTEX(4)

      DATA  MUSTAN / 3, 30, 31, 32, 16*0 /
      DATA  MUSTFL / 3, 4, 5, 6, 7, 5*0 /
      DATA  LISTEX / 3,  7, 4,4  /


      CALL JZIN (CHNAME, 1, 3, LISTEX)

      CALL MZWORK (IXSTOR, MWSP(7),MWSP(24), 0)
      CALL UCOPY  (LC1,LX1,4)
      CALL UCOPY  (MUSTFL,MWSP(7),4)

      CALL TJMUAN (CHNAME,MUSTAN)
      CALL TJMUFL (CHNAME,MUSTFL)

      NENT = IQ(LQSV+2)
      CALL JZTELL (NENT/4)

      CALL TJIDLE (4)

      CALL TCC1
      CALL TJMUAN (CHNAME,MUSTAN)
      CALL TJMUFL (CHNAME,MUSTFL)
      CALL TJWSP  (CHNAME,MWSP(7),MWSP(24), LC1, MUSTFL)

      CALL JZOUT (CHNAME)
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
