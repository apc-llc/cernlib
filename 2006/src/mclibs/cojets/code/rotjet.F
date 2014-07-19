*
* $Id: rotjet.F,v 1.1.1.1 1996/01/11 14:14:42 mclareni Exp $
*
* $Log: rotjet.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:42  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE ROTJET
C     *****************
C-- ROTATES PJSET (CALLED BY JETQCD) - CONTROLLED BY SAME ROTATION
C-- PARAMETERS AS ROTQUA
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
#include "cojets/jetset.inc"
#include "cojets/rotq.inc"
      IF(IROT.EQ.1) RETURN
      IF(IROT.EQ.2) GO TO 10
      PZ1=PJSET(3,NJSET)*CT-PJSET(1,NJSET)*ST
      PJSET(1,NJSET)=PJSET(1,NJSET)*CT+PJSET(3,NJSET)*ST
      PJSET(3,NJSET)=PZ1
      PX1=PJSET(1,NJSET)*CPH-PJSET(2,NJSET)*SPH
      PJSET(2,NJSET)=PJSET(2,NJSET)*CPH+PJSET(1,NJSET)*SPH
      PJSET(1,NJSET)=PX1
      RETURN
10    PJSET(3,NJSET)=-PJSET(3,NJSET)
      RETURN
      END
