*
* $Id: tbbb.F,v 1.1.1.1 1996/03/06 10:47:02 mclareni Exp $
*
* $Log: tbbb.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:02  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE TBBB

C-    Test processor level 1

      CHARACTER    CHNAME*4
      PARAMETER   (CHNAME = 'TBBB')

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

      DATA  MUSTAN / 2, 20, 21, 17*0 /
      DATA  MUSTFL / 2, 3, 77, 5, 6, 5*0 /
      DATA  LISTEX / 3, 24, 4,4  /


      CALL JZIN (CHNAME, 1, 2, LISTEX)

      CALL MZWORK (IXSTOR, MWSP,MWSP(48), 0)
      CALL UCOPY  (LB1,LX1,4)
      CALL UCOPY  (MUSTAN,MWSP,4)

      CALL TJMUAN (CHNAME,MUSTAN)
      CALL TJMUFL (CHNAME,MUSTFL)

      NENT = IQ(LQSV+2)
      CALL JZTELL (NENT/4)

      CALL TJIDLE (4)

      CALL TBB1
      CALL TJMUAN (CHNAME,MUSTAN)
      CALL TJMUFL (CHNAME,MUSTFL)
      CALL TJWSP  (CHNAME,MWSP,MWSP(48), LB1, MUSTAN)

      CALL TBB2
      IF (IQ(LQDW+1).EQ.7)  MUSTFL(3) = 7
      CALL TJMUAN (CHNAME,MUSTAN)
      CALL TJMUFL (CHNAME,MUSTFL)
      CALL TJWSP  (CHNAME,MWSP,MWSP(48), LB1, MUSTAN)

      CALL TBB1
      CALL TJMUAN (CHNAME,MUSTAN)
      CALL TJMUFL (CHNAME,MUSTFL)
      CALL TJWSP  (CHNAME,MWSP,MWSP(48), LB1, MUSTAN)

      CALL JZOUT (CHNAME)
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
