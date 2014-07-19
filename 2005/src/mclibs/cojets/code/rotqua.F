*
* $Id: rotqua.F,v 1.1.1.1 1996/01/11 14:14:42 mclareni Exp $
*
* $Log: rotqua.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:42  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE ROTQUA
C     *****************
C-- ROTATES QUANTA (CALLED BY JTBOOK) - ROTATION PARAMETERS SET IN BJETS
C-- INSET AND TEVOL
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
#include "cojets/parqua.inc"
#include "cojets/rotq.inc"
      IF(IROT.EQ.1) RETURN
      IF(IROT.EQ.2) GO TO 10
      PZ1=PARQUA(NQUA,3)*CT-PARQUA(NQUA,1)*ST
      PARQUA(NQUA,1)=PARQUA(NQUA,1)*CT+PARQUA(NQUA,3)*ST
      PARQUA(NQUA,3)=PZ1
      PX1=PARQUA(NQUA,1)*CPH-PARQUA(NQUA,2)*SPH
      PARQUA(NQUA,2)=PARQUA(NQUA,2)*CPH+PARQUA(NQUA,1)*SPH
      PARQUA(NQUA,1)=PX1
      RETURN
10    PARQUA(NQUA,3)=-PARQUA(NQUA,3)
      RETURN
      END
