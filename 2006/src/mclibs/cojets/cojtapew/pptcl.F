*
* $Id: pptcl.F,v 1.1.1.1 1996/01/11 14:15:03 mclareni Exp $
*
* $Log: pptcl.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:03  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION PPTCL(J,I)
C     *******************
C-- MOCKS ARRAY PPTCL( , ) IN EDIT
C-- CREATED: 88/05/08
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/event.inc"
#include "cojets/itapes.inc"
      PPTCL=PARHAD(I,J)
      RETURN
      END
