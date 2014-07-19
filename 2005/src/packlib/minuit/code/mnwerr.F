*
* $Id: mnwerr.F,v 1.1.1.1 1996/03/07 14:31:32 mclareni Exp $
*
* $Log: mnwerr.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:32  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNWERR
#include "minuit/d506dp.inc"
CC          Calculates the WERR, external parameter errors,
CC      and the global correlation coefficients, to be called
CC      whenever a new covariance matrix is available.
CC
#include "minuit/d506cm.inc"
C                         calculate external error if v exists
      IF (ISW(2) .GE. 1) THEN
      DO 100 L= 1, NPAR
        NDEX = L*(L+1)/2
        DX = SQRT(ABS(VHMAT(NDEX)*UP))
        I = NEXOFI(L)
        IF (NVARL(I) .GT. 1)  THEN
          AL = ALIM(I)
          BA = BLIM(I) - AL
          DU1 = AL + 0.5 *(SIN(X(L)+DX) +1.0) * BA - U(I)
          DU2 = AL + 0.5 *(SIN(X(L)-DX) +1.0) * BA - U(I)
          IF (DX .GT. 1.0)  DU1 = BA
          DX = 0.5 * (ABS(DU1) + ABS(DU2))
        ENDIF
        WERR(L) = DX
  100 CONTINUE
      ENDIF
C                          global correlation coefficients
      IF (ISW(2) .GE. 1) THEN
         DO 130 I= 1, NPAR
            GLOBCC(I) = 0.
            K1 = I*(I-1)/2
            DO 130 J= 1, I
               K = K1 + J
               P(I,J) = VHMAT(K)
  130          P(J,I) = P(I,J)
         CALL MNVERT(P,MAXINT,MAXINT,NPAR,IERR)
         IF (IERR .EQ. 0)   THEN
            DO 150 IIN= 1, NPAR
               NDIAG = IIN*(IIN+1)/2
               DENOM = P(IIN,IIN)*VHMAT(NDIAG)
               IF (DENOM.LE.ONE .AND. DENOM.GE.ZERO)  THEN
                   GLOBCC(IIN) = 0.
               ELSE
                   GLOBCC(IIN) = SQRT(1.0-1.0/DENOM)
               ENDIF
  150       CONTINUE
         ENDIF
      ENDIF
      RETURN
      END
