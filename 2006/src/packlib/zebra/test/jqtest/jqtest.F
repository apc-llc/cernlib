*
* $Id: jqtest.F,v 1.1.1.1 1996/03/06 10:47:02 mclareni Exp $
*
* $Log: jqtest.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:02  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE JQTEST

C-    Test processors control routine, level 0

      CHARACTER    CHNAME*4
      PARAMETER   (CHNAME = 'ROOT')

#include "zebra/quest.inc"
#include "test_include/testla.inc"
#include "test_include/testll.inc"
#include "tejqdd.inc"
#include "test_include/testiq.inc"
#include "test_include/testee.inc"
#include "zebra/jzuc.inc"
C--------------    End CDE                             --------------

      PARAMETER   ( NLOOP = 48 )

      DIMENSION    MUSTAN(20), MUSTFL(10), LISTEX(6)
      DATA  MUSTAN / 4, 1, 2, 3, 4, 15*0 /
      DATA  MUSTFL / 0, 1, 2, 3, 4, 5, 4*0 /
      DATA  LISTEX / 5, 4,8, 4,10, 0 /


C----              Initialize the test store

      CALL CQSTOR (0)
      CALL CQDIVI (2)
      CALL MZLOGL (0,0)

C--                Lift a structure

      CALL CQLODS (0,0)
      CALL UCOPY (LA1,LAREX,12)
      CALL UCOPY (LB4,LAREY,12)
      CALL MZWIPE (IXSTOR+1)

C--                Read the titles

      CALL TZINIT (16,IXDV19)

      CALL JZINIT (IXSTOR, CHNAME, 'T', 2,4,10, LISTEX)

      IF (IQUEST(1).LT.0)  CALL UCOPY (MUSTAN(2),IQ(LQAN+1),IQ(LQAN))

      CALL JZSETF ('TBBB', 3, 77)

      CALL MZWORK (IXSTOR, LIWSXX,MWSP(16), 0)
      CALL UCOPY  (LC1,LX1,4)
      CALL UCOPY  (MUSTFL,LIWSXX,4)

      DO 49   JLOOP=1,NLOOP

      CALL TAAA
      CALL TJMUAN (CHNAME,MUSTAN)
      CALL TJMUFL (CHNAME,MUSTFL)
      CALL TJWSP  (CHNAME,LIWSXX,MWSP(16), LC1, MUSTFL)

      CALL TBBB
      CALL TJMUAN (CHNAME,MUSTAN)
      CALL TJMUFL (CHNAME,MUSTFL)
      CALL TJWSP  (CHNAME,LIWSXX,MWSP(16), LC1, MUSTFL)

      CALL JZZERO (0,2)

      CALL TCCC
      CALL TJMUAN (CHNAME,MUSTAN)
      CALL TJMUFL (CHNAME,MUSTFL)
      CALL TJWSP  (CHNAME,LIWSXX,MWSP(16), LC1, MUSTFL)

      CALL TBBB
      CALL TJMUAN (CHNAME,MUSTAN)
      CALL TJMUFL (CHNAME,MUSTFL)
      CALL TJWSP  (CHNAME,LIWSXX,MWSP(16), LC1, MUSTFL)

      IF (JLOOP.EQ.24)  CALL JZTRAC (0)

   49 CONTINUE
      CALL TJIDLE (40)

      CALL JZEND
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
