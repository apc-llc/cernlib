*
* $Id: cqmota.F,v 1.1.1.1 1996/03/06 10:47:00 mclareni Exp $
*
* $Log: cqmota.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:00  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE CQMOTA (NOCHK,IFLAG)

C-    Dump memory occupation table

#include "test_include/cqc.inc"
#include "zebra/mqsys.inc"
#include "zebra/mzct.inc"
C--------------    End CDE                             --------------
      DIMENSION    IV(12)


      IF (NOCHK.LT.0)  IQCOPT(8)=-7

      CALL CQHEAD (1,'.', 'DUMP MEMORY OCCUPATION TABLE  -----.'
     +,            'OOOJJJJ'
     +,          'MQDVGA,MQDVWI,MQDVAC,JQSTMV,JQDVM1,JQDVM2,NQDVMV.')

      IV(1) = MQDVGA
      IV(2) = MQDVWI
      IV(3) = MQDVAC
      IV(4) = JQSTMV
      IV(5) = JQDVM1
      IV(6) = JQDVM2
      IV(7) = NQDVMV
      CALL CQDATA (IV,IV(7))

      CALL CQHEAD (0,  '.', '.', 'JIIIIII'
     +,          'IQTNMV,JQGAPM,/LQWKTB,NQWKTB,/LQMTA,/LQMTE.')

      IV(1) = IQTNMV
      IV(2) = JQGAPM
      IV(3) = LQWKTB
      IV(4) = NQWKTB
      IV(5) = LQMTA
      IV(6) = LQMTE
      CALL CQDATA (IV,IV(6))

      CALL CQHEAD (0,  '.', '.', 'JJJJJJJ'
     +,  'ADR,DIV,ACTIV,SHIFT,FBANK,LBANK,FENTRY,LENTRY,FREE.')

      LMT = LQMTA
   21 IV(1) = LMT - LQMTA
      IV(2) = LQ(LMT)
      IV(3) = LQ(LMT+1)
      IV(4) = LQ(LMT+2)
      IV(5) = LQ(LMT+3)
      IV(6) = LQ(LMT+4)
      IV(7) = LQ(LMT+5)
      IV(8) = LQ(LMT+6)
      IV(9) = LQ(LMT+7)
      CALL CQDATA (IV(1),IV(9))

      LMT = LMT + 8
      IF (LMT.LT.LQMTE)            GO TO 21
      IF (NOCHK.NE.0)  IQCOPT(8) = 0
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
