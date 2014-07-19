*
* $Id: spectj.F,v 1.1.1.1 1996/01/11 14:14:42 mclareni Exp $
*
* $Log: spectj.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:42  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE SPECTJ
C     *****************
C-- COMPLETES CURRENT CONTENT OF PARHAD( ,7) WITH SPECTATOR JETS
C-- CONTRIBUTION SO AS TO HAVE 4-MOMENTUM CONSERVATION
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      CALL HADRON(1,0,1)
      RETURN
      END
