*
* $Id: isjtqf.F,v 1.1.1.1 1996/01/11 14:15:03 mclareni Exp $
*
* $Log: isjtqf.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:03  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION ISJTQF(IFL)
C     ********************
C-- GIVES ISAJET CODE FOR THE 4 INITIAL AND FINAL TOTAL "JETS"
C-- INPUT: STANDARD INTERNAL PARTON CODE (0 FOR GLUONS)
C-- CREATED: 88/05/08
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      ISJTQF=2*ABS(IFL)
      IF(IFL.LT.0) ISJTQF=ISJTQF+1
      RETURN
      END
