*
* $Id: cqlrta.F,v 1.1.1.1 1996/03/06 10:47:00 mclareni Exp $
*
* $Log: cqlrta.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:00  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE CQLRTA (NOCHK,IFLAG)

C-    Dump link relocation table

#include "test_include/cqc.inc"
#include "zebra/mqsys.inc"
#include "zebra/mzct.inc"
C--------------    End CDE                             --------------
      DIMENSION    IV(12)


      IF (NOCHK.LT.0)  IQCOPT(8)=-7

      CALL CQHEAD (1,  '.', 'DUMP LINK RELOCATION TABLE  -----.'
     +,            'IIIIJJJ'
     +,    '/LQRTA,/LQTA,/LQTE,/LQRTE,JQGAPR,LFIXLO,LFIXHI.')

      IV(1) = LQRTA
      IV(2) = LQTA
      IV(3) = LQTE
      IV(4) = LQRTE
      IV(5) = JQGAPR
      IV(6) = LQ(LQTA-1)
      IV(7) = LQ(LQTE)
      CALL CQDATA (IV,IV(7))

      IF (LQRTA.GE.LQRTE)          GO TO 99

      CALL CQHEAD (0,  '.', '.', 'IJJIJJ.  '
     +,    'LOC,START,END,RELOC,FLAG,LOC,START,END,RELOC,FLAG.')

      N = 10
      L = LQTA

   31 IV(1) = L - LQRTA
      IV(2) = LQ(L)
      IV(3) = LQ(L+1)
      IV(4) = LQ(L+2)
      IV(5) = LQ(L+3)
      IF (L+4.GE.LQTE)             GO TO 34

      IV(6) = IV(1) + 4
      IV(7) = LQ(L+4)
      IV(8) = LQ(L+5)
      IV(9) = LQ(L+6)
      IV(10)= LQ(L+7)
      L = L + 8
      IF (L.GE.LQTE)               GO TO 35
      CALL CQDATA (IV,IV(10))
      GO TO 31

   34 N = 5
   35 CALL CQDATA (IV,IV(N))

      IF (IFLAG.LT.0)              GO TO 99

      CALL CQHEAD (1,  '.', 'LAST MZTABC PARAMETERS.', 'IIJJJJJ'
     +,          '/LQTC1,/LQTC2,LQMTC1,LQMTC2,NQFRTC,NQLIVE.')

      IV(1) = LQTC1
      IV(2) = LQTC2
      IV(3) = LQMTC1
      IV(4) = LQMTC2
      IV(5) = NQFRTC
      IV(6) = NQLIVE
      CALL CQDATA (IV,IV(6))
   99 IF (NOCHK.NE.0)  IQCOPT(8) = 0
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
