*
* $Id: hqpois.F,v 1.1.1.1 1996/01/16 17:08:05 mclareni Exp $
*
* $Log: hqpois.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:05  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/02 18/12/92  14.23.02  by  John Allison
*-- Author :
      SUBROUTINE HQPOIS (SIGCOV, DDERIV, DJN, DSIGA, DDSIGA, N, CHISQ,
     +ALOGLI, IHQERR)
      INTEGER N
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION SIGCOV (N, N)
      DOUBLE PRECISION DDERIV (N, N)
      DOUBLE PRECISION DJN (N), DSIGA (N), DDSIGA (N)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL SIGCOV (N, N)
      REAL DDERIV (N, N)
      REAL DJN (N), DSIGA (N), DDSIGA (N)
#endif
      INTEGER IHQERR
      REAL CHISQ, ALOGLI
* Perform unconstrained Poisson (binned) likelihood fit to bin contents.
* IHQERR = 0 if all's OK.
* CHISQ is calculated by comparing the maximum likelihood solution to the trial
*   histogram data; hence it is not the minimum chi-squared.
* ALOGLI is log likelihood.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcqcor.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcunit.inc"
 
      CHARACTER*80 CHQMES
      LOGICAL PPRINT
      LOGICAL EVALDD, DESPRT
      INTEGER J, K, L, LB, LB0, NWW
      INTEGER IX, IY, IZ
      INTEGER NSECS, ITER, ITERDD, ITRMX0, ITERMX, IDDINT
      REAL OPS, UFACT
      REAL V (NDMAX)
      REAL X, Y, Z, FF, FFMIN, STEP, STEPMI
      EQUIVALENCE (X, V (1)), (Y, V(2)), (Z, V(3))
#if defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 2)
      DOUBLE PRECISION DCHISQ, DLOGLI, DLOGL0, DCHEK1, DCHEK2, DN
      DOUBLE PRECISION VEXPDN
      DOUBLE PRECISION HQDN, HQDJN
      DOUBLE PRECISION SS
#endif
#if !defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 1)
      REAL DCHISQ, DLOGLI, DLOGL0, DCHEK1, DCHEK2, DN
      REAL VEXPDN
      REAL HQDN, HQDJN
      REAL SS
#endif
 
      PARAMETER (ITRMX0 = 50, IDDINT = 3)
      PARAMETER (UFACT = 110000.)
      PARAMETER (DCHEK1 = 1.D-3)
      PARAMETER (DCHEK2 = 10. * DCHEK1)
      PARAMETER (STEPMI = 1. / 32.)
 
 
 
      CHISQ  = 0.
      ALOGLI = 0.
      IHQERR = 0
 
* Set function flag for exponential function.
      IMQFUN = 2
 
* Calculate constant factor.
      VCONST = VOLBIN
 
      OPS = FLOAT (NSIG ** 2) * FLOAT (NBTOT)
      PPRINT = VPRINT
      IF (PPRINT) THEN
         WRITE (LOUT, 10000) IDMQ
         IF (NDIM .EQ. 1) THEN
            WRITE (LOUT, 10100) NSIG, NBTOT
         ELSE IF (NDIM .EQ. 2) THEN
            WRITE (LOUT, 10200) NSIG, NBTOT, NX, NY
         ELSE IF (NDIM .EQ. 3) THEN
            WRITE (LOUT, 10300) NSIG, NBTOT, NX, NY, NZ
         ELSE
            GO TO 430
         END IF
         NSECS = OPS / UFACT
         WRITE (LOUT, 10400) NSECS, 10 * NSECS
         WRITE (LOUT, 10500) ITRMX0
      END IF
 
* Estimate starting values.
* First a least squares fit to linear sum of multiquadrics to get function
*   values (actually only needed for empty bins).
      IF (PPRINT) WRITE (LOUT,10600)
      CALL HQLSQU (SIGCOV, DJN, N, CHISQ, IHQERR)
      IF (IHQERR .NE. 0) GO TO 470
      IF (PPRINT) WRITE (LOUT, 10700) CHISQ
* Rebin.
      CALL HQBIN (NBINS, 1, IHQERR)
      IF (IHQERR .NE. 0) GO TO 470
* Unconstrained, linear least squares fit to log of (contents/VCONST) for
*   starting values.  This is an ad hoc method of getting starting values.
*   Some of the parameters were tuned by experience, and do not have much
*   foundation.  E.g., FFMIN is the minimum value substituted for empty bins
*   (to avoid log (0) = -INF) - otherwise a value from the previous step is
*   used.  Also I tried various algorithms for the variance of empty bins but
*   leaving it at 1 seems OK.
      IF (PPRINT) WRITE (LOUT, 10800)
      FFMIN  = 1. / NMQFUL
      IF (NDIM .EQ. 1) THEN
         DO 10    IX = 1, NX
            X = (IX - 0.5) / NX
            L = IX
            IF (Q (L1H + L) .EQ. 0.) THEN
               FF = HQDN (V)
               FF = MAX (FFMIN, FF)
               Q (L1H + L) = LOG (FF / VCONST)
*              Q (L1V + L) is already 1., as required.
            ELSE
               Q (L1V + L) = Q (L1V + L) / (Q (L1H + L)) ** 2
               Q (L1H + L) = LOG (Q (L1H + L) / VCONST)
            END IF
   10    CONTINUE
      ELSE IF (NDIM .EQ. 2) THEN
         DO 30    IX = 1, NX
            X = (IX - 0.5) / NX
            DO 20    IY = 1, NY
               Y = (IY - 0.5) / NY
               L = (IY - 1) * NX + IX
               IF (Q (L2H + L) .EQ. 0.) THEN
                  FF = HQDN (V)
                  FF = MAX (FFMIN, FF)
                  FF = MIN (1., FF)
                  Q (L2H + L) = LOG (FF / VCONST)
*                 Q (L2V + L) is already 1., as required.
               ELSE
                  Q (L2V + L) = Q (L2V + L) / (Q (L2H + L)) ** 2
                  Q (L2H + L) = LOG (Q (L2H + L) / VCONST)
               END IF
   20       CONTINUE
   30    CONTINUE
      ELSE IF (NDIM .EQ. 3) THEN
         DO 60    IX = 1, NX
            X = (IX - 0.5) / NX
            DO 50    IY = 1, NY
               Y = (IY - 0.5) / NY
               DO 40    IZ = 1, NZ
                  Z = (IZ - 0.5) / NZ
                  L = (IZ - 1) * NX * NY + (IY - 1) * NX + IX
                  IF (Q (L3H + L) .EQ. 0.) THEN
                     FF = HQDN (V)
                     FF = MAX (FFMIN, FF)
                     Q (L3H + L) = LOG (FF / VCONST)
*                    Q (L3V + L) is already 1., as required.
                  ELSE
                     Q (L3V + L) = Q (L3V + L) / (Q (L3H + L)) ** 2
                     Q (L3H + L) = LOG (Q (L3H + L) / VCONST)
                  END IF
   40          CONTINUE
   50       CONTINUE
   60    CONTINUE
      ELSE
         GO TO 430
      END IF
      CALL HQLSQU (SIGCOV, DJN, N, CHISQ, IHQERR)
      IF (IHQERR .NE. 0) GO TO 470
      IF (PPRINT) WRITE (LOUT, 10900) CHISQ
* Rebin.
      CALL HQBIN (NBINS, 1, IHQERR)
      IF (IHQERR .NE. 0) GO TO 470
 
* OK, let's go.
      IF (PPRINT) WRITE (LOUT,11000)
 
      IF (LHQDJN .NE. 0) THEN
* Fill working space with phi_j for each bin.
         IF (PPRINT) WRITE (LOUT, 11100)
         IF (NDIM .EQ. 1) THEN
            DO 80    IX = 1, NX
               X = (IX - 0.5) / NX
               L = IX
               LB0 = NSIG * (L - 1)
               DO 70    K = 1, NSIG
                  LB = NWW * (LB0 + K - 1) + 1
                  SS = HQDJN (V, K)
                  CALL UCOPY (SS, Q (LHQDJN + LB), NWW)
   70          CONTINUE
   80       CONTINUE
         ELSE IF (NDIM .EQ. 2) THEN
            DO 110   IX = 1, NX
               X = (IX - 0.5) / NX
               DO 100   IY = 1, NY
                  Y = (IY - 0.5) / NY
                  L = (IY - 1) * NX + IX
                  LB0 = NSIG * (L - 1)
                  DO 90    K = 1, NSIG
                     LB = NWW * (LB0 + K - 1) + 1
                     SS = HQDJN (V, K)
                     CALL UCOPY (SS, Q (LHQDJN + LB), NWW)
   90             CONTINUE
  100          CONTINUE
  110       CONTINUE
         ELSE IF (NDIM .EQ. 3) THEN
            DO 150   IX = 1, NX
               X = (IX - 0.5) / NX
               DO 140   IY = 1, NY
                  Y = (IY - 0.5) / NY
                  DO 130   IZ = 1, NZ
                     Z = (IZ - 0.5) / NZ
                     L = (IZ - 1) * NX * NY + (IY - 1) * NX + IX
                     LB0 = NSIG * (L - 1)
                     DO 120   K = 1, NSIG
                        LB = NWW * (LB0 + K - 1) + 1
                        SS = HQDJN (V, K)
                        CALL UCOPY (SS, Q (LHQDJN + LB), NWW)
  120                CONTINUE
  130             CONTINUE
  140          CONTINUE
  150       CONTINUE
         ELSE
            GO TO 430
         END IF
      END IF
 
* Begin iterating.
      DESPRT = .FALSE.
      DLOGL0 = -1.D30
      STEP   = 1.
      ITER   = 0
      ITERMX = ITRMX0
      ITERDD = IDDINT
      GO TO 340
 
* Start of iteration loop
  160 CONTINUE
      ITER   = ITER   + 1
      ITERDD = ITERDD + 1
      IF (ITER .GT. ITERMX) THEN
         WRITE (LOUT, 11200)
         GO TO 420
      END IF
  170 CONTINUE
      EVALDD = ITERDD .GE. IDDINT .OR. STEP .LT. 1.
      IF (EVALDD) ITERDD = 0
 
* Evaluate the matrices A and B and solve A * SIGA = B.
      CALL UZERO (DDSIGA, 1, NWW * NSIG)
      IF (EVALDD) CALL UZERO (DDERIV, 1, NWW * NSIG ** 2)
* Calculate DDSIGA and DDERIV lower triangle.
      IF (NDIM .EQ. 1) THEN
         DO 200   IX = 1, NX
            X = (IX - 0.5) / NX
            L = IX
            VEXPDN = VCONST * EXP (HQDN (V))
            LB0 = NSIG * (L - 1)
            DO 190   K = 1, NSIG
               IF (LHQDJN .NE. 0) THEN
                  LB = NWW * (LB0 + K - 1) + 1
                  CALL UCOPY (Q (LHQDJN + LB), SS, NWW)
               ELSE
                  SS = HQDJN (V, K)
               END IF
               DDSIGA (K) = DDSIGA (K) + (Q (L1H + L) - VEXPDN) * SS
               IF (EVALDD) THEN
                  DJN (K) = SS
                  DO 180   J = 1, K
                     DDERIV (J, K) = DDERIV (J, K) + VEXPDN * DJN (K) *
     +               DJN (J)
  180             CONTINUE
               END IF
  190       CONTINUE
  200    CONTINUE
      ELSE IF (NDIM .EQ. 2) THEN
         DO 240   IX = 1, NX
            X = (IX - 0.5) / NX
            DO 230   IY = 1, NY
               Y = (IY - 0.5) / NY
               L = (IY - 1) * NX + IX
               VEXPDN = VCONST * EXP (HQDN (V))
               LB0 = NSIG * (L - 1)
               DO 220   K = 1, NSIG
                  IF (LHQDJN .NE. 0) THEN
                     LB = NWW * (LB0 + K - 1) + 1
                     CALL UCOPY (Q (LHQDJN + LB), SS, NWW)
                  ELSE
                     SS = HQDJN (V, K)
                  END IF
                  DDSIGA (K) = DDSIGA (K) + (Q (L2H + L) - VEXPDN) * SS
                  IF (EVALDD) THEN
                     DJN (K) = SS
                     DO 210   J = 1, K
                        DDERIV (J, K) = DDERIV (J, K) + VEXPDN * DJN
     +                  (K) * DJN (J)
  210                CONTINUE
                  END IF
  220          CONTINUE
  230       CONTINUE
  240    CONTINUE
      ELSE IF (NDIM .EQ. 3) THEN
         DO 290   IX = 1, NX
            X = (IX - 0.5) / NX
            DO 280   IY = 1, NY
               Y = (IY - 0.5) / NY
               DO 270   IZ = 1, NZ
                  Z = (IZ - 0.5) / NZ
                  L = (IZ - 1) * NX * NY + (IY - 1) * NX + IX
                  VEXPDN = VCONST * EXP (HQDN (V))
                  LB0 = NSIG * (L - 1)
                  DO 260   K = 1, NSIG
                     IF (LHQDJN .NE. 0) THEN
                        LB = NWW * (LB0 + K - 1) + 1
                        CALL UCOPY (Q (LHQDJN + LB), SS, NWW)
                     ELSE
                        SS = HQDJN (V, K)
                     END IF
                     DDSIGA (K) = DDSIGA (K) + (Q (L3H + L) - VEXPDN) *
     +               SS
                     IF (EVALDD) THEN
                        DJN (K) = SS
                        DO 250   J = 1, K
                           DDERIV (J, K) = DDERIV (J, K) + VEXPDN *
     +                     DJN (K) * DJN (J)
  250                   CONTINUE
                     END IF
  260             CONTINUE
  270          CONTINUE
  280       CONTINUE
  290    CONTINUE
      ELSE
         GO TO 430
      END IF
* Fill DDERIV upper triangle.
      IF (EVALDD) THEN
         DO 310   K = 1, NSIG
            DO 300   J = 1, K - 1
               DDERIV (K, J) = DDERIV (J, K)
  300       CONTINUE
  310    CONTINUE
      END IF
* Copy to DSIGA and SIGCOV ready for inversion.
      CALL UCOPY (DDSIGA, DSIGA, NWW * NSIG)
      CALL UCOPY (DDERIV, SIGCOV, NWW * NSIG ** 2)
 
* Call CERN library routine DEQINV (F010) for simultaneous equations to
*   solve A * DSIGA = B.
#if defined(CERNLIB_DOUBLE)
      CALL DEQINV (NSIG, SIGCOV, NSIG, DJN, IHQERR, 1, DSIGA)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL REQINV (NSIG, SIGCOV, NSIG, DJN, IHQERR, 1, DSIGA)
#endif
      IF (IHQERR .NE. 0) GO TO 440
 
* Update SIGA.
  320 CONTINUE
      DO 330   J = 1, NSIG
         SIGA (J) = SIGA (J) + STEP * DSIGA (J)
  330 CONTINUE
 
* Evaluate log likelihood for convergence testing, and chi-squared.
  340 CONTINUE
      DLOGLI = 0.
      DCHISQ = 0.
      IF (NDIM .EQ. 1) THEN
         DO 350   IX = 1, NX
            X = (IX - 0.5) / NX
            L = IX
            DN = HQDN (V)
            VEXPDN = VCONST * EXP (DN)
            DLOGLI = DLOGLI + Q (L1H + L) * DN - VEXPDN
            DCHISQ = DCHISQ + (Q (L1H + L) - VEXPDN) ** 2 /
     +      Q (L1V + L)
  350    CONTINUE
      ELSE IF (NDIM .EQ. 2) THEN
         DO 370   IX = 1, NX
            X = (IX - 0.5) / NX
            DO 360   IY = 1, NY
               Y = (IY - 0.5) / NY
               L = (IY - 1) * NX + IX
               DN = HQDN (V)
               VEXPDN = VCONST * EXP (DN)
               DLOGLI = DLOGLI + Q (L2H + L) * DN - VEXPDN
               DCHISQ = DCHISQ + (Q (L2H + L) - VEXPDN) ** 2 /
     +         Q (L2V + L)
  360       CONTINUE
  370    CONTINUE
      ELSE IF (NDIM .EQ. 3) THEN
         DO 400   IX = 1, NX
            X = (IX - 0.5) / NX
            DO 390   IY = 1, NY
               Y = (IY - 0.5) / NY
               DO 380   IZ = 1, NZ
                  Z = (IZ - 0.5) / NZ
                  L = (IZ - 1) * NX * NY + (IY - 1) * NX + IX
                  DN = HQDN (V)
                  VEXPDN = VCONST * EXP (DN)
                  DLOGLI = DLOGLI + Q (L3H + L) * DN - VEXPDN
                  DCHISQ = DCHISQ + (Q (L3H + L) - VEXPDN) ** 2 /
     +            Q (L3V + L)
  380          CONTINUE
  390       CONTINUE
  400    CONTINUE
      ELSE
         GO TO 430
      END IF
      IF (PPRINT) THEN
         IF (ITER .EQ. 0) WRITE (LOUT, 11300)
         WRITE (LOUT, 11400) ITER, DLOGLI, DCHISQ
      END IF
 
* Check convergence.
      IF (DLOGLI - DLOGL0 .GT. DCHEK1) THEN
         DLOGL0 = DLOGLI
         IF (STEP .LT. 1.) STEP = 2. * STEP
         GO TO 160
      ELSE IF (DLOGLI - DLOGL0 .LT. -DCHEK2) THEN
* Decreasing log liklelihood - back up...
         DO 410   J = 1, NSIG
            SIGA (J) = SIGA (J) - STEP * DSIGA (J)
  410    CONTINUE
         IF (STEP .GE. STEPMI) THEN
* ...and try smaller step, re-evaluating 2nd derivative matrix next time...
            STEP = 0.5 * STEP
            GO TO 320
         ELSE
            IF (DESPRT) THEN
               GO TO 450
            ELSE
* ...or, indesperation, re-evaluate 2nd derivative now.
               DESPRT = .TRUE.
               GO TO 170
            END IF
         END IF
      END IF
 
* Set output variables.
  420 CONTINUE
      CHISQ  = DCHISQ
      ALOGLI = DLOGLI
 
      GO TO 470
 
  430 CONTINUE
      WRITE (CHQMES, '(I3, '' dimensions not programmed yet.'')') NDIM
      IHQERR = 20
      GO TO 460
  440 CONTINUE
      WRITE (CHQMES, '(''Error'', I5, '' in DEQINV'')') IHQERR
      GO TO 460
  450 CONTINUE
      WRITE (CHQMES, '(''Likelihood decreasing in spite of rescue'',
     +'' operations!'')')
      IHQERR = 40
      GO TO 460
 
  460 CONTINUE
      CALL HBUG (CHQMES, 'HQPOIS', IDMQ)
 
  470 CONTINUE
 
10000 FORMAT (1X, 'Multiquadric smoothing of histogram/ntuple', I7)
10100 FORMAT (I6, ' parameters for', I7, ' bins.')
10200 FORMAT (I6, ' parameters for', I7, ' bins (', I3, ' x', I3, ').')
10300 FORMAT (I6, ' parameters for', I7, ' bins (', I3, ' x', I3, ' x',
     +I3, ').')
10400 FORMAT (3X, 'This will take anything from', I10, ' to', I10,
     +' CERN-unit-secs.')
10500 FORMAT (3X, 'We allow, initially,', I4, ' iterations.')
10600 FORMAT (1X, 'First a least squares fit to a sum of multiquadrics'/
     +3X, '(to get function values for next step).')
10700 FORMAT (3X, 'Chi-squared was:', G12.5)
10800 FORMAT (
     +1X, 'Next a least squares fit of log of contents to a sum of '/
     +3X, 'multiquadrics (with help of function values derived above).')
10900 FORMAT (3X, 'Chi-squared was:', G12.5)
11000 FORMAT (
     +1X, 'Now an unconstrained Poisson likelihood fit of binned'/
     +3X, 'data to exponential of sum of multiquadrics.')
11100 FORMAT (3X, '(Storing function values to speed subsequent ',
     +'calculation.)')
11200 FORMAT (1X, 'Not converged - result accepted nevertheless.')
11300 FORMAT (1X,
     +'HQPOIS: Iteration  log likelihood         chi-squared')
11400 FORMAT (I16, 2X, 2G20.13)
 
      END
