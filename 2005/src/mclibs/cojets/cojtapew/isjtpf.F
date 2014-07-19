*
* $Id: isjtpf.F,v 1.1.1.1 1996/01/11 14:15:02 mclareni Exp $
*
* $Log: isjtpf.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:02  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION ISJTPF(KP)
C     *******************
C-- GIVES ISAJET CODE FOR THE 4 INITIAL AND FINAL TOTAL "JETS"
C-- INPUT: INTERNAL ID FOR LEPTONS
C-- CREATED: 88/05/08
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/data1.inc"
#include "cojets/itapes.inc"
      ID=IDENTF(KP)
      ISJTPF=2*(ABS(ID)-10)+12
      IF(ID.LT.0) ISJTPF=ISJTPF+1
      RETURN
      END
