*
* $Id: bufout.F,v 1.1.1.1 1996/01/11 14:15:02 mclareni Exp $
*
* $Log: bufout.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:02  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE BUFOUT(IL)
C     *********************
C-- ADAPTED FROM ISAJET
C
C          INVERSE OF BUFIN.
C          WRITE OUT ZEVEL.
C
C          NRECS INCREMENTED BY 1
C          IL SET TO 3
C-- CREATED: 88/05/08
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/final.inc"
#include "cojets/idrun.inc"
#include "cojets/itapes.inc"
#include "cojets/zevel.inc"
      ITA=ABS(ITEVT)
C          STANDARD FORTRAN 77 WRITE.
      IZEVEL(2)=IL
      WRITE(ITA) ZEVEL
      IL=3
      NRECS=NRECS+1
      RETURN
      END
