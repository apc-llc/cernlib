*
* $Id: mncalf.F,v 1.1.1.1 1996/03/07 14:31:28 mclareni Exp $
*
* $Log: mncalf.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:28  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNCALF(FCN,PVEC,YCALF,FUTIL)
#include "minuit/d506dp.inc"
CC        Called only from MNIMPR.  Transforms the function FCN
CC        by dividing out the quadratic part in order to find further
CC        minima.    Calculates  ycalf = (f-fmin)/(x-xmin)*v*(x-xmin)
CC
#include "minuit/d506cm.inc"
      EXTERNAL FCN,FUTIL
      DIMENSION PVEC(15)
      NPARX = NPAR
      CALL MNINEX(PVEC)
      CALL FCN(NPARX,GIN,F,U,4,FUTIL)
      NFCN = NFCN + 1
      DO 200 I= 1, NPAR
      GRD(I) = 0.
         DO 200 J= 1, NPAR
         M = MAX(I,J)
         N = MIN(I,J)
         NDEX = M*(M-1)/2 + N
  200    GRD(I) = GRD(I) + VTHMAT(NDEX) * (XT(J)-PVEC(J))
      DENOM = 0.
      DO 210 I= 1, NPAR
  210 DENOM = DENOM + GRD(I) * (XT(I)-PVEC(I))
      IF (DENOM .LE. ZERO)  THEN
         DCOVAR = 1.
         ISW(2) = 0
         DENOM = 1.0
      ENDIF
      YCALF = (F-APSI) / DENOM
      RETURN
      END
