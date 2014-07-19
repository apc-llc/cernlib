*
* $Id: hqmxli.F,v 1.1.1.1 1996/01/16 17:08:05 mclareni Exp $
*
* $Log: hqmxli.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:05  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/02 18/12/92  11.32.44  by  Rene Brun
*-- Author :
      SUBROUTINE HQMXLI (SIGCOV, DWK1, DSIGA, N, CHISQ, ALOGLI, IHQERR)
      INTEGER N
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION SIGCOV (N, N)
      DOUBLE PRECISION DWK1 (N), DSIGA (N)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL SIGCOV (N, N)
      REAL DWK1 (N), DSIGA (N)
#endif
      INTEGER IHQERR
      REAL CHISQ, ALOGLI
* Performs maximum likelihood fit to events using extended likelihood method.
*   Assumes good starting values have already been calculated.
* CHISQ is calculated by comparing the maximum likelihood solution to the trial
*   histogram data; hence it is not the minimum chi-squared.
* ALOGLI is log likelihood.
* IHQERR = 0 of all's OK.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcqcor.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcunit.inc"
 
      CHARACTER*80 CHQMES
      LOGICAL PPRINT
      INTEGER I, J, K, NSECS, ITER, ITERMX, NWW, L, IX, IY, IZ
      REAL T, OPS, UFACT, STEP, STEPMI
      REAL V (NDMAX)
      REAL X, Y, Z
      EQUIVALENCE (X, V (1)), (Y, V(2)), (Z, V(3))
#if defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 2)
      DOUBLE PRECISION DCHISQ, DLOGLI, DLOGL0
      DOUBLE PRECISION DCHEK1, DCHEK2
      DOUBLE PRECISION INTGRG, HQD, HQDJ, HQDN
      DOUBLE PRECISION DV (NDMAX), DGMLT1, DGMLT2, DGMLT3
#endif
#if !defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 1)
      REAL DCHISQ, DLOGLI, DLOGL0
      REAL DCHEK1, DCHEK2
      REAL INTGRG, HQD, HQDJ, HQDN
      REAL DV (NDMAX), RGMLT1, RGMLT2, RGMLT3
#endif
      EXTERNAL HQMXA1, HQMXA2, HQMXA3
      EXTERNAL HQMXB1, HQMXB2, HQMXB3
      EXTERNAL HQMXC1, HQMXC2, HQMXC3
 
      PARAMETER (ITERMX = 20)
      PARAMETER (UFACT = 40000.)
      PARAMETER (DCHEK1 = 1.E-3)
      PARAMETER (DCHEK2 = 100. * DCHEK1)
      PARAMETER (STEPMI = 1. / 32.)
 
      CHISQ  = 0.
      ALOGLI = 0.
      IHQERR   = 0
 
* Set function flag for exponential function.
      IMQFUN = 2
 
      OPS = FLOAT (NSIG ** 2) * FLOAT (NBTOT)
      PPRINT = VPRINT
      IF (PPRINT) THEN
         WRITE (LOUT, 10000) NSIG, NBTOT
         NSECS = OPS / UFACT
         WRITE (LOUT, 10100) NSECS
         CALL TIMED (T)
      END IF
 
* Begin iterating.
      DLOGL0 = -1.D30
      STEP = 1.
      ITER = 0
      GO TO 90
 
* Start of iteration loop
   10 CONTINUE
      ITER = ITER + 1
      IF (ITER .GT. ITERMX) GO TO 190
 
* Evaluate the matrices A and B (use SIGCOV and SIGA respectively) and
*   solve A * SIGA = B.
      CALL UZERO (DSIGA,  1, NWW * NSIG)
      CALL UZERO (SIGCOV, 1, NWW * NSIG ** 2)
 
* Calculate SIGA and SIGCOV lower triangle.
      DO 40    K = 1, NSIG
* Sum over events of phi_j...
         DO 20    I = 1, NMQEVS
            CALL HGNF (IDMQ, I, V, IHQERR)
            IF (IHQERR .NE. 0) GO TO 200
            DSIGA (K) = DSIGA (K) + HQDJ (V, K)
   20    CONTINUE
* Integral e^s phi_j dx...
         KMQ = K
#if defined(CERNLIB_DOUBLE)
         IF (NDIM .EQ. 1) THEN
            INTGRG = DGMLT1 (HQMXB1, DBLE (XMI), DBLE (XMA), NINTVS,
     +      NQUADS, DV)
         ELSE IF (NDIM .EQ. 2) THEN
            INTGRG = DGMLT2 (HQMXB2, DBLE (YMI), DBLE (YMA), NINTVS,
     +      NQUADS, DV)
         ELSE IF (NDIM .EQ. 3) THEN
            INTGRG = DGMLT3 (HQMXB3, DBLE (ZMI), DBLE (ZMA), NINTVS,
     +      NQUADS, DV)
#endif
#if !defined(CERNLIB_DOUBLE)
         IF (NDIM .EQ. 1) THEN
            INTGRG = RGMLT1 (HQMXB1, XMI, XMA, NINTVS, NQUADS, DV)
         ELSE IF (NDIM .EQ. 2) THEN
            INTGRG = RGMLT2 (HQMXB2, YMI, YMA, NINTVS, NQUADS, DV)
         ELSE IF (NDIM .EQ. 3) THEN
            INTGRG = RGMLT3 (HQMXB3, ZMI, ZMA, NINTVS, NQUADS, DV)
#endif
         ELSE
            GO TO 180
         END IF
         DSIGA (K) = DSIGA (K) - INTGRG
         DO 30    J = 1, K
* Integral e^s phi_j phi_k dx...
            JMQ = J
#if defined(CERNLIB_DOUBLE)
            IF (NDIM .EQ. 1) THEN
               INTGRG = DGMLT1 (HQMXC1, DBLE (XMI), DBLE (XMA), NINTVS,
     +         NQUADS, DV)
            ELSE IF (NDIM .EQ. 2) THEN
               INTGRG = DGMLT2 (HQMXC2, DBLE (YMI), DBLE (YMA), NINTVS,
     +         NQUADS, DV)
            ELSE IF (NDIM .EQ. 3) THEN
               INTGRG = DGMLT3 (HQMXC3, DBLE (ZMI), DBLE (ZMA), NINTVS,
     +         NQUADS, DV)
#endif
#if !defined(CERNLIB_DOUBLE)
            IF (NDIM .EQ. 1) THEN
               INTGRG = RGMLT1 (HQMXC1, XMI, XMA, NINTVS, NQUADS, DV)
            ELSE IF (NDIM .EQ. 2) THEN
               INTGRG = RGMLT2 (HQMXC2, YMI, YMA, NINTVS, NQUADS, DV)
            ELSE IF (NDIM .EQ. 3) THEN
               INTGRG = RGMLT3 (HQMXC3, ZMI, ZMA, NINTVS, NQUADS, DV)
#endif
            ELSE
               GO TO 180
            END IF
            SIGCOV (J, K) = SIGCOV (J, K) + INTGRG
   30    CONTINUE
   40 CONTINUE
 
* Fill SIGCOV upper triangle.
      DO 60    K = 1, NSIG
         DO 50    J = 1, K - 1
            SIGCOV (K, J) = SIGCOV (J, K)
   50    CONTINUE
   60 CONTINUE
 
* Call CERN library routine DEQINV (F010) for simultaneous equations to
*   solve A * SIGA = B.
#if defined(CERNLIB_DOUBLE)
      CALL DEQINV (NSIG, SIGCOV, NSIG, DWK1, IHQERR, 1, DSIGA)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL REQINV (NSIG, SIGCOV, NSIG, DWK1, IHQERR, 1, DSIGA)
#endif
      IF (IHQERR .NE. 0) GO TO 230
* Update SIGA.
   70 CONTINUE
      DO 80    J = 1, NSIG
         SIGA (J) = SIGA (J) + STEP * DSIGA (J)
   80 CONTINUE
 
* Evaluate log likelihood for convergence testing.
   90 CONTINUE
      DLOGLI = 0.
      DO 100   I = 1, NMQEVS
         CALL HGNF (IDMQ, I, V, IHQERR)
         IF (IHQERR .NE. 0) GO TO 200
         DLOGLI = DLOGLI + HQD (V)
  100 CONTINUE
* Integral s dx, where s = sum of multiquadrics.
#if defined(CERNLIB_DOUBLE)
      IF (NDIM .EQ. 1) THEN
         INTGRG = DGMLT1 (HQMXA1, DBLE (XMI), DBLE (XMA),
     +   NINTVS, NQUADS, DV)
      ELSE IF (NDIM .EQ. 2) THEN
         INTGRG = DGMLT2 (HQMXA2, DBLE (YMI), DBLE (YMA),
     +   NINTVS, NQUADS, DV)
      ELSE IF (NDIM .EQ. 3) THEN
         INTGRG = DGMLT3 (HQMXA3, DBLE (ZMI), DBLE (ZMA),
     +   NINTVS, NQUADS, DV)
#endif
#if !defined(CERNLIB_DOUBLE)
      IF (NDIM .EQ. 1) THEN
         INTGRG = RGMLT1 (HQMXA1, XMI, XMA, NINTVS, NQUADS, DV)
      ELSE IF (NDIM .EQ. 2) THEN
         INTGRG = RGMLT2 (HQMXA2, YMI, YMA, NINTVS, NQUADS, DV)
      ELSE IF (NDIM .EQ. 3) THEN
         INTGRG = RGMLT3 (HQMXA3, ZMI, ZMA, NINTVS, NQUADS, DV)
#endif
      ELSE
         GO TO 180
      END IF
      DLOGLI = DLOGLI - INTGRG
 
* Check convergence.
      IF (DLOGLI - DLOGL0 .GT. DCHEK1) THEN
         DLOGL0 = DLOGLI
         IF (STEP .LT. 1.) STEP = 2. * STEP
         GO TO 10
      ELSE IF (DLOGLI - DLOGL0 .LT. -DCHEK2) THEN
* Decreasing log liklelihood - back up and try smaller step
         IF (STEP .LT. STEPMI) GO TO 210
         DO 110   J = 1, NSIG
            SIGA (J) = SIGA (J) - STEP * DSIGA (J)
  110    CONTINUE
         STEP = 0.5 * STEP
         GO TO 70
      END IF
 
* Check convergence.
      IF (DLOGLI - DLOGL0 .GT. DCHEK1) THEN
         DLOGL0 = DLOGLI
         GO TO 10
      ELSE IF (DLOGLI - DLOGL0 .LT. -DCHEK2) THEN
         GO TO 210
      END IF
 
* Get chi-squared.
      DCHISQ = 0.
      IF (NDIM .EQ. 1) THEN
         DO 120   IX = 1, NX
            X = (IX - 0.5) / NX
            L = IX
            DCHISQ = DCHISQ + (Q (L1H + L) - HQDN (V)) ** 2 /
     +      Q (L1V + L)
  120    CONTINUE
      ELSE IF (NDIM .EQ. 2) THEN
         DO 140   IX = 1, NX
            X = (IX - 0.5) / NX
            DO 130   IY = 1, NY
               Y = (IY - 0.5) / NY
               L = (IY - 1) * NX + IX
               DCHISQ = DCHISQ + (Q (L2H + L) - HQDN (V)) ** 2 /
     +         Q (L2V + L)
  130       CONTINUE
  140    CONTINUE
      ELSE IF (NDIM .EQ. 3) THEN
         DO 170   IX = 1, NX
            X = (IX - 0.5) / NX
            DO 160   IY = 1, NY
               Y = (IY - 0.5) / NY
               DO 150   IZ = 1, NZ
                  Z = (IZ - 0.5) / NZ
                  L = (IZ - 1) * NX * NY + (IY - 1) * NX + IX
                  DCHISQ = DCHISQ + (Q (L3H + L) - HQDN (V)) ** 2 /
     +            Q (L3V + L)
  150          CONTINUE
  160       CONTINUE
  170    CONTINUE
      ELSE
         GO TO 10
      END IF
      CHISQ = DCHISQ
      ALOGLI = DLOGLI
*     IF (PPRINT) THEN
*        CALL TIMED (T)
*        WRITE (LOUT, '(3X, ''Took'', G12.5, '' secs.'')') T
*     END IF
 
      GO TO 230
 
  180 CONTINUE
      WRITE (CHQMES, '(I3, '' dimensions not programmed yet.'')') NDIM
      IHQERR = 10
      GO TO 220
  190 CONTINUE
      WRITE (CHQMES, '(''More than'', I4, '' iterations.'')') ITERMX
      IHQERR = 20
      GO TO 220
  200 CONTINUE
      WRITE (CHQMES, '(''Error'', I3, '' in HGNF.'')') IHQERR
      GO TO 220
  210 CONTINUE
      WRITE (CHQMES, '(''Likelihood decreasing!'')')
      IHQERR = 30
      GO TO 220
 
  220 CONTINUE
      CALL HBUG (CHQMES, 'HQMXLI', IDMQ)
 
  230 CONTINUE
 
10000 FORMAT (1X, 'Multiquadric unbinned maximum likelihood fit to ',
     +'events with', I5, ' parameters for', I7, ' bins.')
10100 FORMAT (3X, 'This will take about', I10, ' CERN-unit-secs.')
 
      END
