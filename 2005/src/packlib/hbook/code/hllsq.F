*
* $Id: hllsq.F,v 1.1.1.1 1996/01/16 17:07:42 mclareni Exp $
*
* $Log: hllsq.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:42  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
      SUBROUTINE HLLSQ(NDATA,A0,A1,IFAIL)
*.==========>
*.           Auxiliary to HFITPO/HFITEX.
*            Extracted from CERN Program library routine LLSQ
*.
*.  Least square linear fit without weights
*.  (added to LSQ by B. Schorr, 15.02.1982.)
*..=========> ( R.Brun from CERNLIB )
      DIMENSION EXDA(4)
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION    XBAR, YBAR, X2BAR, XYBAR, DET
#endif
      N=ABS(NDATA)
      IFAIL=-2
      XBAR=0.
      YBAR=0.
      X2BAR=0.
      XYBAR=0.
      DO 10 I=1,N
         CALL HFITH1(EXDA,I)
         XK=EXDA(3)
         YK=EXDA(1)
         IF(NDATA.LT.0)THEN
            IF(YK.LE.0.)YK=1.E-9
            YK=LOG(YK)
         ENDIF
         XBAR=XBAR+XK
         YBAR=YBAR+YK
         X2BAR=X2BAR+XK**2
         XYBAR=XYBAR+XK*YK
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
