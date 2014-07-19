*
* $Id: pallsq.F,v 1.1.1.1 1996/03/01 11:38:41 mclareni Exp $
*
* $Log: pallsq.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:41  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  1.15/00 28/08/92  16.16.18  by  Rene Brun
*-- Author :
      SUBROUTINE PALLSQ(NDATA,X,Y,A0,A1,IFAIL)
*.==========>
*.           Auxiliary to PAFITV
*            Extracted from CERN Program library routine LLSQ
*.
*.  Least square linear fit without weights
*.  (added to LSQ by B. Schorr, 15.02.1982.)
*..=========> ( R.Brun from CERNLIB )
      DIMENSION X(*),Y(*)
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
            XK=X(I)
            YK=Y(I)
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
