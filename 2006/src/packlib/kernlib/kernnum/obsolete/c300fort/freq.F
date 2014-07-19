*
* $Id: freq.F,v 1.1.1.1 1996/02/15 17:48:17 mclareni Exp $
*
* $Log: freq.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:17  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
      FUNCTION FREQ(RX)
#if defined(CERNLIB_NUMLOPRE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL FREQ,RX,ROUND
#endif
#if defined(CERNLIB_NUMOWNCODE)
      EXTERNAL ERF,ERFC,DERF,DERFC
#endif
C
      DATA CONST/ 0.70710 67811 86548 D0 /
C     ( CONST=SQRT(1/2). )
C
#if defined(CERNLIB_NUME2465)
      DATA XMAX/ 75.3 D0 /
#endif
#if defined(CERNLIB_NUME293)
      DATA XMAX/ 25.8 D0 /
#endif
#if defined(CERNLIB_NUME75)
      DATA XMAX/ 13.0 D0 /
#endif
#if defined(CERNLIB_NUME38)
      DATA XMAX/ 8.9 D0 /
#endif
#if defined(CERNLIB_NUME999)
      DATA XMAX/ *** NOT AVAILABLE *** /
#endif
C     ( XMAX=SQRT(-ALOG(RMIN)-10.0), WHERE RMIN IS THE SMALLEST NORMAL-
C     IZED REPRESENTABLE NUMBER.  ERFC(XMAX) IS CLOSE TO THE UNDERFLOW
C     THRESHOLD. )
C
C     ******************************************************************
C
#if defined(CERNLIB_NUMLOPRE)
C  STATEMENT FUNCTION.
      ROUND(D)=SNGL(D+(D-DBLE(SNGL(D))))
C
C     ******************************************************************
C
#endif
#if defined(CERNLIB_NUMHIPRE)
C  START.
      CX=CONST*RX
      IF(CX.LT.-XMAX) THEN
         FREQ=0.
      ELSE
         IF(CX.GE.0.) FREQ=0.5+0.5*ERF(CX)
         IF(CX.LT.0.) FREQ=0.5*ERFC(-CX)
      ENDIF
#endif
#if defined(CERNLIB_NUMLOPRE)
C  START.
      CX=CONST*DBLE(RX)
      IF(CX.LT.-XMAX) THEN
         FREQ=0.D0
      ELSE
         IF(CX.GE.0.D0) FREQ = ROUND( 0.5D0 + 0.5D0*DERF(CX) )
         IF(CX.LT.0.D0) FREQ = ROUND( 0.5D0*DERFC(-CX) )
      ENDIF
#endif
      RETURN
      END
