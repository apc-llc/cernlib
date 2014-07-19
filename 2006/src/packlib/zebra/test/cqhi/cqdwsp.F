*
* $Id: cqdwsp.F,v 1.1.1.1 1996/03/06 10:47:00 mclareni Exp $
*
* $Log: cqdwsp.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:00  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE CQDWSP (NOCHK)

C-    Write working space links

#include "test_include/cqc.inc"
#include "test_include/testla.inc"
#include "test_include/testll.inc"
#include "test_include/testdd.inc"
#include "test_include/testiq.inc"
#include "test_include/testee.inc"
C--------------    End CDE                             --------------


      IF (NOCHK.LT.0)  IQCOPT(8)=-7

      CALL CQHEAD (1,  '.', 'DUMP WSP LINKS  -----.', 'LLLLLLL',
     +                  'LQUSER.')

      CALL CQLINK (0, LQUSER,7)

      CALL CQHEAD (0, '-', '-', '-',
     +                  'LQMAIN,,LA1,,LA2,,LA3,,LA4,,LA5,,LA6.')

      CALL CQLINK (0, LQMAIN,7)

      CALL CQHEAD (0, '-', '-', '-',
     +                  'LB1,,LB2,,LB3,,LB4.')

      CALL CQLINK (0, LB1,4)

      CALL CQHEAD (0, '-', '-', '-',
     +                  'LC1,,LC2,,LC3,,LC4,,LD1.')

      CALL CQLINK (0, LC1,5)

      CALL CQHEAD (0, '-', '-', '-',
     +                  'LOW1,,LOW2,,LOW3,,LOW4,,LOW5,,LOW6.')

      CALL CQLINK (0, LL1,6)

      CALL CQHEAD (0, '-', '-', '-',
     +                  'LAREN(1),,(2),,(3),,(4),,(5),,(6).')

      CALL CQLINK (0, LAREX,6)
      CALL CQLINK (0, LAREX(7),6)
      CALL CQLINK (0, LAREY,6)
      CALL CQLINK (0, LAREY(7),6)
      CALL CQLINK (0, LAREZ,6)
      CALL CQLINK (0, LAREZ(7),6)
      IF (NOCHK.NE.0)  IQCOPT(8) = 0
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
