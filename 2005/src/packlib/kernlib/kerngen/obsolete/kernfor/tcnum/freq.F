*
* $Id: freq.F,v 1.1.1.1 1996/02/15 17:54:16 mclareni Exp $
*
* $Log: freq.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:16  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      FUNCTION FREQ(X)
#if defined(CERNLIB_NUMHIPRE)
      DATA CONST/ 0.70710 67811 8655 /
#endif
#if defined(CERNLIB_NUMLOPRE)
      DATA CONST/ 0.70710 6781 /
#endif
C     ( CONST=SQRT(1/2). )
C
#if defined(CERNLIB_NUME293)
      DATA XMAX/ 25.8 /
#endif
#if defined(CERNLIB_NUME75)
      DATA XMAX/ 13.0 /
#endif
#if defined(CERNLIB_NUME38)
      DATA XMAX/ 8.9 /
#endif
C     ( XMAX=SQRT(-ALOG(RMIN)-10.0), WHERE RMIN IS THE SMALLEST NORMAL-
C     IZED REPRESENTABLE NUMBER.  ERFC(XMAX) IS CLOSE TO THE UNDERFLOW
C     THRESHOLD. )
C
C  START.
      CX=CONST*X
      FREQ=0.
      IF(CX.LT.-XMAX) RETURN
      IF(CX.GE.0.) FREQ=0.5+0.5*ERF(CX)
      IF(CX.LT.0.) FREQ=0.5*ERFC(-CX)
      RETURN
      END
#ifdef CERNLIB_C300FORT_FREQ
#undef CERNLIB_C300FORT_FREQ
#endif
