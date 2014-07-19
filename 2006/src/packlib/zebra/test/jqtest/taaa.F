*
* $Id: taaa.F,v 1.1.1.1 1996/03/06 10:47:02 mclareni Exp $
*
* $Log: taaa.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:02  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE TAAA

C-    Test processor level 1

      CHARACTER    CHNAME*4
      PARAMETER   (CHNAME = 'TAAA')

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

      DATA  MUSTAN / 1, 10, 18*0 /
      DATA  MUSTFL / 1, 2, 3, 4, 5, 5*0 /
      DATA  LISTEX / 3, 12, 4,4  /


      CALL JZIN (CHNAME, 1, 1, LISTEX)

#if defined(CERNLIB_TESTLOG)
      IF (IQUEST(1).EQ.0)  CALL JZLOG ('A')
#endif

      CALL MZWORK (IXSTOR, MWSP(3),MWSP(24), 0)
      CALL UCOPY  (LA1,LX1,4)
      CALL UCOPY  (MUSTFL,MWSP(3),4)

      CALL TJMUAN (CHNAME,MUSTAN)
      CALL TJMUFL (CHNAME,MUSTFL)

      NENT = IQ(LQSV+2)
      CALL JZTELL (NENT/4)

      CALL TJIDLE (4)

#if defined(CERNLIB_TESTLOG)
      IF (NENT.EQ.3)  CALL JZLOG ('TAX')
      IF (NENT.EQ.4)  CALL JZLOG ('.')
#endif

      CALL TAA1
      CALL TJMUAN (CHNAME,MUSTAN)
      CALL TJMUFL (CHNAME,MUSTFL)
      CALL TJWSP  (CHNAME,MWSP(3),MWSP(24), LA1, MUSTFL)

      CALL JZOUT (CHNAME)
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
