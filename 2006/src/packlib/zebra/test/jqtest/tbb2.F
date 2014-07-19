*
* $Id: tbb2.F,v 1.1.1.1 1996/03/06 10:47:02 mclareni Exp $
*
* $Log: tbb2.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:02  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE TBB2

C-    Test processor level 2

      CHARACTER    CHNAME*4
      PARAMETER   (CHNAME = 'TBB2')

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

      DATA  MUSTAN / 3, 22, 23, 24, 16*0 /
      DATA  MUSTFL / 221, 222, 223, 224, 225, 5*0 /


      CALL JZIN (CHNAME, 0, 3, 0)
      IF (IQUEST(1).LT.0)  CALL UCOPY (MUSTAN(2),IQ(LQAN+1),IQ(LQAN))

      CALL MZWORK (IXSTOR, LIWSXX,MWSP(24), 0)

      CALL TJMUAN (CHNAME,MUSTAN)
      CALL TJMUFL (CHNAME,MUSTFL)

      NENT = IQ(LQSV+2)
      CALL JZTELL (NENT/4)

      IF (NENT.EQ.7)  THEN
          CALL JZSETF ('TBBB', 3, 7)
          IQ(LQUP+1) = 7
        ENDIF

      CALL TJIDLE (4)

      CALL JZOUT (CHNAME)
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
