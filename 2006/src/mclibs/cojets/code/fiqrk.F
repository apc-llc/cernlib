*
* $Id: fiqrk.F,v 1.1.1.1 1996/01/11 14:14:36 mclareni Exp $
*
* $Log: fiqrk.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:36  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION FIQRK(ALPSQ)
C     *********************
C-- QUARK INTEGRATION FUNCTION USED BY TABLES
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
      CALL ZLIM(EXP(A),QZFLSQ(LFLA),QZSQ,ZLW,ZUP,FLAG)
      IF(FLAG.GT.0.) GO TO 100
      FIQRK=(PAPINT(1,ZUP)-PAPINT(1,ZLW))/(A-ALLAM2)
      RETURN
100   FIQRK=0.
      RETURN
      END
