*
* $Id: rvdist.F,v 1.1.1.1 1996/02/15 17:48:41 mclareni Exp $
*
* $Log: rvdist.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:41  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          REAL FUNCTION       RVDIST(N,X,X2,Y,Y2)
          REAL                X(*), X2(*), Y(*), Y2(*), A, B
          DISTF(A,B)  =  ABS(A - B)
          RVDIST       =  0.
          IF(N .LE. 0)  RETURN
          RVDIST  =  DISTF(X(1),Y(1))
          IF(N .LE. 1)  RETURN
#if (!defined(CERNLIB_NUMUC))&&(!defined(CERNLIB_NUMCR))&&(!defined(CERNLIB_NUMDE))
          JX  =  LOCF(X2) - LOCF(X)
          JY  =  LOCF(Y2) - LOCF(Y)
#endif
#if defined(CERNLIB_NUMUC)||defined(CERNLIB_NUMCR)
          JX  =  LOC(X2) - LOC(X)
          JY  =  LOC(Y2) - LOC(Y)
#endif
#if defined(CERNLIB_NUMDE)
          JX  =  (%LOC(X2) - %LOC(X)) / 4
          JY  =  (%LOC(Y2) - %LOC(Y)) / 4
#endif
          LX  =  1
          LY  =  1
          DO 10     J  =  2, N
             LX  =  LX + JX
             LY  =  LY + JY
             RVDIST  =  AMAX1(RVDIST,DISTF(X(LX),Y(LY)))
  10         CONTINUE
          END
