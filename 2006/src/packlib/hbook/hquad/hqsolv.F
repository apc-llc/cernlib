*
* $Id: hqsolv.F,v 1.1.1.1 1996/01/16 17:08:06 mclareni Exp $
*
* $Log: hqsolv.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:06  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 22/11/92  12.13.37  by  John Allison
*-- Author :
      SUBROUTINE HQSOLV (SIGCOV, DW, N, CHISQ, IHQERR)
      INTEGER N
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION SIGCOV (N, N)
      DOUBLE PRECISION DW (N)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL SIGCOV (N, N)
      REAL DW (N)
#endif
      REAL CHISQ
      INTEGER IHQERR
* Solves for coefficients SIGA given SIGDEN, the estimates of the density at the
*   significant points (converts to bin heights with VOLBIN).
* Works in NORMALISED coordinates.
* CHISQ is chi-squared.
* IHQERR = 0 if all's OK.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcqcor.inc"
#include "hbook/hcbook.inc"
 
      CHARACTER*80 CHQMES
      INTEGER I, J, K, NWW, L, IX, IY, IZ
      REAL V (NDMAX)
      REAL X, Y, Z
      EQUIVALENCE (X, V (1)), (Y, V(2)), (Z, V(3))
#if defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 2)
      DOUBLE PRECISION HQDJN, DCHISQ, HQDN
#endif
#if !defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 1)
      REAL HQDJN, DCHISQ, HQDN
#endif
 
      IHQERR = 0
      IMQFUN = 1
 
* Evaluate the multiquadrics at the significant points (use the covariance
*   matrix SIGCOV to store these and DW as working space).
      DO 30    K = 1, NSIG
         DO 10    I = 1, NDIM
            V (I) = SIGV (K, I)
   10    CONTINUE
         DO 20    J = 1, NSIG
            SIGCOV (K, J) = HQDJN (V, J)
   20    CONTINUE
   30 CONTINUE
 
* Transfer the data to SIGA for DEQN, converting to bin heights.
      DO 40    J = 1, NSIG
         SIGA (J) = SIGDEN (J) * VOLBIN
   40 CONTINUE
 
* Call CERN library F010 for simultaneous equations.
#if defined(CERNLIB_DOUBLE)
      CALL DEQN (NSIG, SIGCOV, NSIG, DW, IHQERR, 1, SIGA)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL REQN (NSIG, SIGCOV, NSIG, DW, IHQERR, 1, SIGA)
#endif
      IF (IHQERR .NE. 0) GO TO 110
 
* Clear covariance matrix (it was used as working space) as it has no meaning
*   for interpolation.
      CALL UZERO (SIGCOV, 1, NWW * NSIG ** 2)
 
* Get chi-squared.
      DCHISQ = 0.
      IF (NDIM .EQ. 1) THEN
         DO 50    IX = 1, NX
            X = (IX - 0.5) / NX
            L = IX
            DCHISQ = DCHISQ + (Q (L1H + L) - HQDN (V)) ** 2 /
     +      Q (L1V + L)
   50    CONTINUE
      ELSE IF (NDIM .EQ. 2) THEN
         DO 70    IX = 1, NX
            X = (IX - 0.5) / NX
            DO 60    IY = 1, NY
               Y = (IY - 0.5) / NY
               L = (IY - 1) * NX + IX
               DCHISQ = DCHISQ + (Q (L2H + L) - HQDN (V)) ** 2 /
     +         Q (L2V + L)
   60       CONTINUE
   70    CONTINUE
      ELSE IF (NDIM .EQ. 3) THEN
         DO 100   IX = 1, NX
            X = (IX - 0.5) / NX
            DO 90    IY = 1, NY
               Y = (IY - 0.5) / NY
               DO 80    IZ = 1, NZ
                  Z = (IZ - 0.5) / NZ
                  L = (IZ - 1) * NX * NY + (IY - 1) * NX + IX
                  DCHISQ = DCHISQ + (Q (L3H + L) - HQDN (V)) ** 2 /
     +            Q (L3V + L)
   80          CONTINUE
   90       CONTINUE
  100    CONTINUE
      ELSE
         GO TO 110
      END IF
      CHISQ = DCHISQ
 
      GO TO 120
 
  110 CONTINUE
      WRITE (CHQMES, '(''Error'', I3, '' in DEQN'')') IHQERR
      CALL HBUG (CHQMES, 'HQSOLV', IDMQ)
 
  120 CONTINUE
 
      END
