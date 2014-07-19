*
* $Id: outh.F,v 1.1.1.1 1996/01/11 14:15:03 mclareni Exp $
*
* $Log: outh.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:03  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE OUTH
C     ***************
C-- INTERFACES COJETS/COJTAPEW TO WRITE END RECORD
C-- CREATED: 88/05/08
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/evint.inc"
#include "cojets/final.inc"
#include "cojets/idrun.inc"
#include "cojets/iflghv.inc"
#include "cojets/itapes.inc"
#include "cojets/nkinpt.inc"
#include "cojets/nleave.inc"
#include "cojets/total.inc"
#include "cojets/weakon.inc"
C
      IF(LEV(1).EQ.1) THEN
        NKINF=NKINPT
      ELSEIF(LEV(1).GE.2.AND.LEV(1).LE.4) THEN
        NKINF=NGEX
      ENDIF
      SIGF=SIGTOT
      NEVTAP=IEVT
      ALUM=NEVTAP/SIGTOT
      ACCEPT=FLOAT(IEVT)/FLOAT(NEVNTS+NHVREJ)
C
      CALL WREND
      RETURN
      END
