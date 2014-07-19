*
* $Id: q2gen.F,v 1.1.1.1 1996/01/11 14:14:41 mclareni Exp $
*
* $Log: q2gen.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:41  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE Q2GEN(IFLA,YIN,Y,QSQ)
C     ********************************
C-- GENERATES EVOLUTION PARAMETER OF LEG (Y.GE.YIN)
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/alqgen.inc"
#include "cojets/itapes.inc"
#include "cojets/qcds.inc"
C
      LFLA=1
      IF(IFLA.EQ.LGLUS) LFLA=2
      Y=-LOG(CJRN(0.))/PROFLA(LFLA)+YIN
      ALQX=EXP(Y)
      IF(ALQX.GT.ALQF) ALQX=ALQF
      QSQ=EXP(ALQX*ALQZM+ALLAM2)
      RETURN
      END
