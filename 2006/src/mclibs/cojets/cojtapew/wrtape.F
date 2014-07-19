*
* $Id: wrtape.F,v 1.1.1.1 1996/01/11 14:15:04 mclareni Exp $
*
* $Log: wrtape.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:04  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE WRTAPE
C     *****************
C-- ADAPTED FROM ISAJET
C
C          WRITE OUT MONTECARLO DATA IF EDIT IS TRUE
C
C-- CREATED: 88/05/08
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/idrun.inc"
#include "cojets/itapes.inc"
      LOGICAL EDIT
C
C-- WRITE EVENT IF EDIT IS .TRUE.
      IF(EDIT(I)) THEN
        CALL WGENS
      ELSE
        IEVT=IEVT-1
      ENDIF
      RETURN
      END
