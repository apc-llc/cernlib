*
* $Id: hlnfct.F,v 1.1.1.1 1996/01/16 17:07:56 mclareni Exp $
*
* $Log: hlnfct.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:56  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 23/10/92  11.51.54  by  R. J. Genik II
*-- Author :    R. J. Genik II   23/10/92
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION FUNCTION HLNFCT(X)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL FUNCTION HLNFCT(X)
#endif
C----------------------------------------------------------------------
C-
C-   Purpose : CALCULATES LOG OF X! WITH 1ST ORDER CORREECTION
C-             TO STIRLING APPROXIMATION
C-
C-   Returned value  : (DOUBLE PRECISION) LOG OF X!
C-   Inputs  : X  integers
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created   17-OCT-1992   R. J. Genik II
C----------------------------------------------------------------------
C
      INTEGER X
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION DXPHLF,DXPONE,DLRT2P
#endif
      PARAMETER (DLRT2P =  0.9189385332047)
C
C----------------------------------------------------------------------
C
C     DLRT2P = LOG(SQRT(2*pi)), used in Stirling
C     First check if we need to calc, else return 1 for x=0,1, or
C     zero for x<0
C----------------------------------------------------------------------
C
      IF (X.GT.1) THEN
      DXPHLF = X + 0.5
      DXPONE = X+1.
      HLNFCT = DXPHLF*LOG(DXPONE) - DXPONE + DLRT2P
     +  + LOG(1. + 1./(12.*DXPONE))
      ELSEIF (X.LT.0) THEN
        HLNFCT = 0.
      ELSE
        HLNFCT = 1.
      ENDIF
      END
