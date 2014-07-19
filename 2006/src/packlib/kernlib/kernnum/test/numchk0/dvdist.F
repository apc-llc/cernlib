*
* $Id: dvdist.F,v 1.1.1.1 1996/02/15 17:48:41 mclareni Exp $
*
* $Log: dvdist.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:41  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          REAL FUNCTION       DVDIST(N,X,X2,Y,Y2)
          DOUBLE PRECISION    X(*), X2(*), Y(*), Y2(*), A, B
          DISTF(A,B)  =  ABS(SNGL(A - B))
          DVDIST       =  0.
          IF(N .LE. 0)  RETURN
          DVDIST  =  DISTF(X(1),Y(1))
          IF(N .LE. 1)  RETURN
#if (!defined(CERNLIB_NUMUC))&&(!defined(CERNLIB_NUMCR))&&(!defined(CERNLIB_NUMDE))
          JX  =  (LOCF(X2) - LOCF(X)) / 2
          JY  =  (LOCF(Y2) - LOCF(Y)) / 2
#endif
#if defined(CERNLIB_NUMUC)||defined(CERNLIB_NUMCR)
          JX  =  (LOC(X2) - LOC(X)) / 2
          JY  =  (LOC(Y2) - LOC(Y)) / 2
#endif
#if defined(CERNLIB_NUMDE)
          JX  =  (%LOC(X2) - %LOC(X)) / 8
          JY  =  (%LOC(Y2) - %LOC(Y)) / 8
#endif
          LX  =  1
          LY  =  1
          DO 10     J  =  2, N
             LX  =  LX + JX
             LY  =  LY + JY
             DVDIST  =  AMAX1(DVDIST,DISTF(X(LX),Y(LY)))
  10         CONTINUE
          END
