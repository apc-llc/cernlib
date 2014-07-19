*
* $Id: figlu.F,v 1.1.1.1 1996/01/11 14:14:36 mclareni Exp $
*
* $Log: figlu.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:36  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION FIGLU(ALPSQ)
C     *********************
C-- GLUON INTEGRATION FUNCTION USED BY TABLES
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/cutoff.inc"
#include "cojets/fint.inc"
#include "cojets/itapes.inc"
#include "cojets/qcd.inc"
C
      A=ALPSQ
      PSQ=EXP(A)
      CALL ZLIMSY(PSQ,QZSQ,ZLW,ZUP,FLAG)
      FIGLU=PAPINT(2,ZUP)-PAPINT(2,ZLW)
      DO 1 L=1,NFLAVT
      CALL ZLIMSY(PSQ,QZFLSQ(L),ZLW,ZUP,FLAG)
      IF(FLAG.GT.0) GO TO 2
1     FIGLU=FIGLU+PAPINT(3,ZUP)-PAPINT(3,ZLW)
2     FIGLU=FIGLU/(A-ALLAM2)
      RETURN
      END
