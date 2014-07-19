*
* $Id: gecode.F,v 1.1.1.1 1996/01/11 14:14:37 mclareni Exp $
*
* $Log: gecode.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:37  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE GECODE(ICHING,IFLING)
C     ********************************
C-- CHANNEL --+ PARTON PAIR FLAVORS CODE
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/alqgen.inc"
#include "cojets/itapes.inc"
      INTEGER ICHING(2),IFLING(2)
      INTEGER IPCOD(7),IPBCOD(7)
      DATA IPCOD/1,-1,2,-2,3,-3,7/, IPBCOD/-1,1,-2,2,-3,3,7/
      IFLING(2)=IPCOD(ICHING(2))
      IF(IPBAR.EQ.1) GO TO 10
      IFLING(1)=IPCOD(ICHING(1))
      RETURN
10    IFLING(1)=IPBCOD(ICHING(1))
      RETURN
      END
