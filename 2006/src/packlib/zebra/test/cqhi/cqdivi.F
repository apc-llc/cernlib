*
* $Id: cqdivi.F,v 1.1.1.1 1996/03/06 10:47:00 mclareni Exp $
*
* $Log: cqdivi.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:00  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE CQDIVI (NDIV)

C-    Create NDIV low and NDIV high divisions

#include "test_include/testll.inc"
#include "test_include/testdd.inc"
#include "test_include/testiq.inc"
C--------------    End CDE                             --------------


      CALL MZSDIV (0,-1)
      CALL MZDIV (IXSTOR,IXDV3,'TESTDIV3',1200,2000, '.')
      CALL MZDIV (IXSTOR,IXDV20,'TDIV20', 2000,4000, 'RP')

      IF (NDIV.LT.2)               RETURN
      CALL MZSDIV (0,-1)
      CALL MZDIV (IXSTOR,IXDV19,'TDIV19', 1200,4000, 'L')
      CALL MZDIV (IXSTOR,IXDV4, 'TDIV04', 1200,4000, 'R')

      IF (NDIV.LT.3)               RETURN
      CALL MZSDIV (0,-1)
      CALL MZDIV (IXSTOR,IXDV5, 'TDIV05', 1200,4000, '.')
      CALL MZDIV (IXSTOR,IXDV18,'TDIV18', 1200,4000, 'RL')
      RETURN
      END
