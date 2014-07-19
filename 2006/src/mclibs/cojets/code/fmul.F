*
* $Id: fmul.F,v 1.1.1.1 1996/01/11 14:14:36 mclareni Exp $
*
* $Log: fmul.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:36  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION FMUL(AN)
C     *****************
C--  GIVES MULTIPLICITY DISTRIBUTION OF SPECTATOR JET MESONS
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/ffmul.inc"
#include "cojets/itapes.inc"
      Z=AN/AVNCH
      FMUL=FMULDG(Z)
      RETURN
      END
