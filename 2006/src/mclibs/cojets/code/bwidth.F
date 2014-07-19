*
* $Id: bwidth.F,v 1.1.1.1 1996/01/11 14:14:33 mclareni Exp $
*
* $Log: bwidth.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:33  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE BWIDTH(BMAS)
C     ***********************
C-- GENERATES BREIT-WIGNER DISTRIBUTION FOR WEAK BOSON MASS
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/data1.inc"
#include "cojets/itapes.inc"
#include "cojets/qcds.inc"
#include "cojets/weakon.inc"
      DATA ZH,WH/2.,3./
C
      BMASOR=SQRT(BIM2)
10    CONTINUE
      X=TAN(PI*(-.5+CJRN(0.)))
      BMAS2=X*BMASOR*WIDTH+BIM2
      IF(BMAS2.LT.0.) GO TO 10
      BMAS=SQRT(BMAS2)
      DMAS=ABS(BMAS-BMASOR)
      IF(DMAS.GT.4.*WIDTH.OR.BMAS.LT.10.) GO TO 10
      IF(IFDC.EQ.0) RETURN
      IF(WEAKON.EQ.ZH.AND.IFDC.EQ.12.AND.BMAS.LT.2.*QMAS(6)+2.)
     *GO TO 10
      IF(WEAKON.EQ.WH.AND.IFDC.EQ.18.AND.BMAS.LT.
     *QMAS(6)+QMAS(5)+2.)
     *GO TO 10
      RETURN
      END
