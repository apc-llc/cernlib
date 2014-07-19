*
* $Id: isbosf.F,v 1.1.1.1 1996/01/11 14:15:02 mclareni Exp $
*
* $Log: isbosf.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:02  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION ISBOSF(KBOS)
C     *********************
C-- GIVES FINAL ISAJET CODE FOR INTERMEDIATE BOSON
C-- INPUT: INT(PBOS(6))
C-- CREATED: 88/05/08
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      DIMENSION ISF(4)
      DATA ISF/1,4,2,3/
      ISBOSF=ISF(KBOS)
      RETURN
      END
