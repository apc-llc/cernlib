*
* $Id: d106fn.F,v 1.1.1.1 1996/02/15 17:48:40 mclareni Exp $
*
* $Log: d106fn.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:40  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
#if defined(CERNLIB_NEVER)
      REAL FUNCTION D106FN(X)
      DOUBLE PRECISION XD,D
      COMMON /D106CM/IFN,A,B
C
C  SINGLE-PRECISION INTEGRAND FOR D106 TESTS.
C
C  STATEMENT FUNCTION.
      RND(D)=SNGL(D+(D-DBLE(SNGL(D))))
C
C  START.
      XD=X
      IF(IFN.EQ.1) D=1D0
      IF(IFN.EQ.2) D=XD**2
      IF(IFN.EQ.3) D=1D0-XD**2
      IF(IFN.GE.4) D=DSIN(XD)
      D106FN=RND(D)
      RETURN
      END
#endif
