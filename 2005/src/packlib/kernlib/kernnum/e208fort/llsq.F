*
* $Id: llsq.F,v 1.1.1.1 1996/02/15 17:48:36 mclareni Exp $
*
* $Log: llsq.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:36  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
      SUBROUTINE LLSQ(NDATA,X,Y,A0,A1,IFAIL)
C
C  LEAST SQUARE LINEAR FIT WITHOUT WEIGHTS
C  (ADDED TO LSQ BY B. SCHORR, 15.02.1982.)
C  (MODIFIED     BY H. LIPPS,   3.10.1983.)
C
      DIMENSION X(2),Y(2)
#if defined(CERNLIB_NUMRDBLE)
      DOUBLE PRECISION    XBAR, YBAR, X2BAR, XYBAR, DET
#endif
      N=NDATA
      IFAIL=-2
      IF(N .LT. 2) RETURN
      XBAR=0.
      YBAR=0.
      X2BAR=0.
      XYBAR=0.
      DO 10 I=1,N
            XBAR=XBAR+X(I)
            YBAR=YBAR+Y(I)
            X2BAR=X2BAR+X(I)**2
            XYBAR=XYBAR+X(I)*Y(I)
   10 CONTINUE
      FN=FLOAT(N)
      DET=FN*X2BAR-XBAR**2
      IFAIL=-1
      IF(DET .LE. 0.) RETURN
      IFAIL=0
      A0=(X2BAR*YBAR-XBAR*XYBAR)/DET
      A1=(FN*XYBAR-XBAR*YBAR)/DET
      RETURN
      END
