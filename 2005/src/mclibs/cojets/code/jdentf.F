*
* $Id: jdentf.F,v 1.1.1.1 1996/01/11 14:14:39 mclareni Exp $
*
* $Log: jdentf.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:39  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION JDENTF(IDINT)
C     **********************
C-- GIVES ISAJET ID CORRESPONDING TO INTERNAL ID OF QUARK/GLUON
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/data1.inc"
#include "cojets/itapes.inc"
#include "cojets/nflav.inc"
      IDINTA=ABS(IDINT)
      IF(IDINTA.LE.NFLAV.AND.IDINTA.GT.0) THEN
        JDENTF=IDINT
      ELSE
        JDENTF=IDGL
      ENDIF
      RETURN
      END
