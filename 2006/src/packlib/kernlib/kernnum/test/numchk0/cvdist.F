*
* $Id: cvdist.F,v 1.1.1.1 1996/02/15 17:48:41 mclareni Exp $
*
* $Log: cvdist.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:41  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          REAL FUNCTION       CVDIST(N,X,X2,Y,Y2)
          COMPLEX             X(*), X2(*), Y(*), Y2(*), A, B
          DISTF(A,B)  =  AMAX1(ABS(REAL(A-B)),ABS(AIMAG(A-B)))
          CVDIST      =  0.
          IF(N .LE. 0)  RETURN
          CVDIST  =  DISTF(X(1),Y(1))
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
             CVDIST  =  AMAX1(CVDIST,DISTF(X(LX),Y(LY)))
  10         CONTINUE
          END
