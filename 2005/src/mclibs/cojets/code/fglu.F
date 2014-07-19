*
* $Id: fglu.F,v 1.1.1.1 1996/01/11 14:14:36 mclareni Exp $
*
* $Log: fglu.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:36  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION FGLU(X)
C     ****************
C-- GLUON MOMENTUM DISTRIBUTION
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/alqgen.inc"
#include "cojets/eicht.inc"
#include "cojets/itapes.inc"
#include "cojets/tabqrk.inc"
      DATA ICALL/0/
      DATA EGLU/5./
C
      IF(ICALL.GT.0) GO TO 10
      ICALL=1
      QE2=QZEV**2
10    CONTINUE
C
      IF(IEICHT.EQ.1) GO TO 71
      IF(IEICHT.EQ.2) GO TO 72
      FGLU=AGLU*(1.+EGLU)*(1.-X)**EGLU
      RETURN
71    FGLU=EICHT1(X,QE2,1)
      RETURN
72    FGLU=EICHT2(X,QE2,1)
      RETURN
      END
