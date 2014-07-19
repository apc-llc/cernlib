*
* $Id: krcomb.F,v 1.1.1.1 1996/01/11 14:14:39 mclareni Exp $
*
* $Log: krcomb.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:39  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION KRCOMB(IFL1,IFL2)
C     **************************
C-- RECOMBINES QUARK-ANTIQUARK AND DIQUARK-QUARK PAIRS INTO PRE-PARTICLE
C-- CODE
C-- CREATED: 88/05/17
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/data1.inc"
#include "cojets/itapes.inc"
      IF(ABS(IFL1).GT.10) GO TO 10
      IF(ABS(IFL2).GT.10) GO TO 20
C-- QUARK-ANTIQUARK
      KRCOMB=MESO(6*(MAX(IFL1,IFL2)-1)-MIN(IFL1,IFL2))
      RETURN
C-- DIQUARK-QUARK
10    KRCOMB=IFL1*10+IFL2
      RETURN
20    KRCOMB=IFL2*10+IFL1
      RETURN
      END
