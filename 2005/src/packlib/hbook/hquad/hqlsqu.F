*
* $Id: hqlsqu.F,v 1.1.1.1 1996/01/16 17:08:03 mclareni Exp $
*
* $Log: hqlsqu.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:03  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/02 18/12/92  11.31.51  by  Rene Brun
*-- Author :
      SUBROUTINE HQLSQU (SIGCOV, DJN, N, CHISQ, IHQERR)
      INTEGER N
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION SIGCOV (N, N)
      DOUBLE PRECISION DJN (N)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL SIGCOV (N, N)
      REAL DJN (N)
#endif
      INTEGER IHQERR
      REAL CHISQ
* Perform unconstrained least squares fit to bin contents.
* IHQERR = 0 if all's OK.
* CHISQ is chi-squared.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcqcor.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcunit.inc"
 
      CHARACTER*80 CHQMES
      LOGICAL PPRINT
      INTEGER J, K, L, IX, IY, IZ, NSECS, JMQFUN, NWW
      REAL OPS, UFACT
*     REAL T
      REAL V (NDMAX)
      REAL X, Y, Z
      EQUIVALENCE (X, V (1)), (Y, V(2)), (Z, V(3))
#if defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 2)
      DOUBLE PRECISION DCHISQ
      DOUBLE PRECISION HQDJN, HQDN
#endif
#if !defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 1)
      REAL DCHISQ
      REAL HQDJN, HQDN
#endif
 
      DATA UFACT /330000./
 
      IHQERR = 0
 
* Preserve IMQFUN
      JMQFUN = IMQFUN
 
      IMQFUN = 1
 
      OPS = FLOAT (NSIG ** 2) * FLOAT (NBTOT)
      PPRINT = (JMQFUN .EQ. 1).AND.VPRINT
      IF (PPRINT) THEN
         WRITE (LOUT, 10000) NSIG, NBTOT
         NSECS = OPS / UFACT
         WRITE (LOUT, 10100) NSECS
*        CALL TIMED (T)
      END IF
 
* Evaluate the matrices A and B (use SIGCOV and SIGA respectively).
* Clear SIGA and SIGCOV lower triangle.
      CALL UZERO (SIGA,   1, NWW * NSIG)
      CALL UZERO (SIGCOV, 1, NWW * NSIG ** 2)
* Calculate SIGA and SIGCOV lower triangle.
      IF (NDIM .EQ. 1) THEN
         DO 30    IX = 1, NX
            X = (IX - 0.5) / NX
            L = IX
            DO 20    K = 1, NSIG
               DJN (K) = HQDJN (V, K)
               SIGA (K) = SIGA (K) +
     +         Q (L1H + L) * DJN (K) / Q (L1V + L)
               DO 10    J = 1, K
                  SIGCOV (J, K) = SIGCOV (J, K) +
     +            DJN (J) * DJN (K) / Q (L1V + L)
   10          CONTINUE
   20       CONTINUE
   30    CONTINUE
      ELSE IF (NDIM .EQ. 2) THEN
         DO 70    IX = 1, NX
            X = (IX - 0.5) / NX
            DO 60    IY = 1, NY
               Y = (IY - 0.5) / NY
               L = (IY - 1) * NX + IX
               DO 50    K = 1, NSIG
                  DJN (K) = HQDJN (V, K)
                  SIGA (K) = SIGA (K) +
     +            Q (L2H + L) * DJN (K) / Q (L2V + L)
                  DO 40    J = 1, K
                     SIGCOV (J, K) = SIGCOV (J, K) +
     +               DJN (J) * DJN (K) / Q (L2V + L)
   40             CONTINUE
   50          CONTINUE
   60       CONTINUE
   70    CONTINUE
      ELSE IF (NDIM .EQ. 3) THEN
         DO 120   IX = 1, NX
            X = (IX - 0.5) / NX
            DO 110   IY = 1, NY
               Y = (IY - 0.5) / NY
               DO 100   IZ = 1, NZ
                  Z = (IZ - 0.5) / NZ
                  L = (IZ - 1) * NX * NY + (IY - 1) * NX + IX
                  DO 90    K = 1, NSIG
                     DJN (K) = HQDJN (V, K)
                     SIGA (K) = SIGA (K) + Q (L3H + L) * DJN (K) / Q
     +               (L3V + L)
                     DO 80    J = 1, K
                        SIGCOV (J, K) = SIGCOV (J, K) + DJN (J) * DJN
     +                  (K) / Q (L3V + L)
   80                CONTINUE
   90             CONTINUE
  100          CONTINUE
  110       CONTINUE
  120    CONTINUE
      ELSE
         GO TO 210
      END IF
* Fill SIGCOV upper triangle.
      DO 140   K = 1, NSIG
         DO 130   J = 1, K - 1
            SIGCOV (K, J) = SIGCOV (J, K)
  130    CONTINUE
  140 CONTINUE
 
* Call CERN library routine for simultaneous equations to solve A * SIGA = B.
* Try DEQINV (F010).
#if defined(CERNLIB_DOUBLE)
      CALL DEQINV (NSIG, SIGCOV, NSIG, DJN, IHQERR, 1, SIGA)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL REQINV (NSIG, SIGCOV, NSIG, DJN, IHQERR, 1, SIGA)
#endif
      IF (IHQERR .NE. 0) GO TO 220
* Try DSEQN (F012) (our matrix is symmetric).
*     CALL DSEQN (NSIG, SIGCOV, NSIG, IHQERR, 1, SIGA)
#if defined(CERNLIB_DOUBLE)
*     CALL DSEQN (NSIG, SIGCOV, NSIG, IHQERR, 1, SIGA)
#endif
#if !defined(CERNLIB_DOUBLE)
*     CALL RSEQN (NSIG, SIGCOV, NSIG, IHQERR, 1, SIGA)
#endif
*     IF (IHQERR .NE. 0) GO TO 20
*     CALL DSFINV (NSIG, SIGCOV, NSIG)
 
* Get chi-squared.
      DCHISQ = 0.
      IF (NDIM .EQ. 1) THEN
         DO 150   IX = 1, NX
            X = (IX - 0.5) / NX
            L = IX
            DCHISQ = DCHISQ + (Q (L1H + L) - HQDN (V)) ** 2 /
     +      Q (L1V + L)
  150    CONTINUE
      ELSE IF (NDIM .EQ. 2) THEN
         DO 170   IX = 1, NX
            X = (IX - 0.5) / NX
            DO 160   IY = 1, NY
               Y = (IY - 0.5) / NY
               L = (IY - 1) * NX + IX
               DCHISQ = DCHISQ + (Q (L2H + L) - HQDN (V)) ** 2 /
     +         Q (L2V + L)
  160       CONTINUE
  170    CONTINUE
      ELSE IF (NDIM .EQ. 3) THEN
         DO 200   IX = 1, NX
            X = (IX - 0.5) / NX
            DO 190   IY = 1, NY
               Y = (IY - 0.5) / NY
               DO 180   IZ = 1, NZ
                  Z = (IZ - 0.5) / NZ
                  L = (IZ - 1) * NX * NY + (IY - 1) * NX + IX
                  DCHISQ = DCHISQ + (Q (L3H + L) - HQDN (V)) ** 2 /
     +            Q (L3V + L)
  180          CONTINUE
  190       CONTINUE
  200    CONTINUE
      ELSE
         GO TO 210
      END IF
      CHISQ = DCHISQ
*     IF (PPRINT) THEN
*        CALL TIMED (T)
*        WRITE (LOUT, '(3X, ''Took'', G12.5, '' secs.'')') T
*     END IF
 
      GO TO 240
 
* Errors.
  210 CONTINUE
      WRITE (CHQMES, '(I3, '' dimensions not programmed yet.'')') NDIM
      IHQERR = 1
      GO TO 230
  220 CONTINUE
      WRITE (CHQMES, '(''Error'', I5, '' in DEQINV'')') IHQERR
*     WRITE (CHQMES, '(''Error'', I5, '' in DSEQN'')') IHQERR
      GO TO 230
 
  230 CONTINUE
      CALL HBUG (CHQMES, 'HQLSQU', IDMQ)
 
  240 CONTINUE
 
*Restore IMQFUN.
      IF (JMQFUN .GT. 0) IMQFUN = JMQFUN
 
10000 FORMAT (1X, 'Multiquadric unconstrained least squares fit with',
     +I5, ' parameters for', I7, ' bins.')
10100 FORMAT (3X, 'This will take about', I10, ' CERN-unit-secs.')
 
      END
