*
* $Id: hqlsqc.F,v 1.1.1.1 1996/01/16 17:08:03 mclareni Exp $
*
* $Log: hqlsqc.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:03  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 22/11/92  15.16.56  by  John Allison
*-- Author :
      SUBROUTINE HQLSQC ( SIGCOV, DW, N, CHISQ, IHQERR)
      INTEGER N
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION SIGCOV (N, N)
      DOUBLE PRECISION DW (N)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL SIGCOV (N, N)
      REAL DW (N)
#endif
      INTEGER IHQERR
      REAL CHISQ
* Perform least squares fit constrained for zero asymptotes.
* For asymptotic zero everywhere NFREE = NSIG - NDIM - 1 and the sum and
*   sum (v*w) is zeroed in HQSSV.
* CHISQ is chi-squared.
* IHQERR = 0 if all's OK.
 
***************************************************************
*
*    Change to avoid MINUIT.   Note: HQMNU no longer exists!!!  (It is
*      attached here for reference.)
*
******************************************************************
 
#include "hbook/hcqcom.inc"
#include "hbook/hcqcor.inc"
#include "hbook/hcbook.inc"
 
      CHARACTER*80 CHQMES
      INTEGER ISIG, JSIG, JMQFUN, L, IX, IY, IZ
      REAL V (NDMAX)
      REAL X, Y, Z
      EQUIVALENCE (X, V (1)), (Y, V(2)), (Z, V(3))
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION DCHISQ, HQDN
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL DCHISQ, HQDN
#endif
*     EXTERNAL HQMNU
 
      IHQERR = 0
 
* Preserve IMQFUN
      JMQFUN = IMQFUN
 
      IMQFUN = 1
      NFREE = NSIG - NDIM - 1
 
* Use HQSOLV for starting values.
      CALL HQSOLV (SIGCOV, DW, N, CHISQ, IHQERR)
      IF (IHQERR .NE. 0) GO TO 130
 
* Initialise MINUIT.
*     CALL MNINIT (5, 6, 7)
 
* Printing from MINUIT?
*     IF (.NOT. TPRINT) THEN
*        CALL MNEXCM (HQMNU, 'SET PRINTOUT', -1.D0, 1, IHQERR, 0)
*        IF (IHQERR .NE. 0) GO TO 10
*     END IF
 
* Define parameters and starting values for MINUIT.
      DO 10    ISIG = 1, NFREE
*        CALL MNPARM (ISIG, 'SIGA', SIGA (ISIG),
*    +   1.D-3, 0.D0, 0.D0, IHQERR)
         IF (IHQERR .NE. 0) GO TO 120
   10 CONTINUE
 
* Minimise.
*     CALL MNEXCM (HQMNU, 'MINIMISE', 0, 0, IHQERR, 0)
      IF (IHQERR .NE. 0) GO TO 120
 
* Get chi-squared.
      DCHISQ = 0.
      IF (NDIM .EQ. 1) THEN
         DO 20    IX = 1, NX
            X = (IX - 0.5) / NX
            L = IX
            DCHISQ = DCHISQ + (Q (L1H + L) - HQDN (V)) ** 2 /
     +      Q (L1V + L)
   20    CONTINUE
      ELSE IF (NDIM .EQ. 2) THEN
         DO 40    IX = 1, NX
            X = (IX - 0.5) / NX
            DO 30    IY = 1, NY
               Y = (IY - 0.5) / NY
               L = (IY - 1) * NX + IX
               DCHISQ = DCHISQ + (Q (L2H + L) - HQDN (V)) ** 2 /
     +         Q (L2V + L)
   30       CONTINUE
   40    CONTINUE
      ELSE IF (NDIM .EQ. 3) THEN
         DO 70    IX = 1, NX
            X = (IX - 0.5) / NX
            DO 60    IY = 1, NY
               Y = (IY - 0.5) / NY
               DO 50    IZ = 1, NZ
                  Z = (IZ - 0.5) / NZ
                  L = (IZ - 1) * NX * NY + (IY - 1) * NX + IX
                  DCHISQ = DCHISQ + (Q (L3H + L) - HQDN (V)) ** 2 /
     +            Q (L3V + L)
   50          CONTINUE
   60       CONTINUE
   70    CONTINUE
      ELSE
         GO TO 120
      END IF
      CHISQ = DCHISQ
 
* Get covariance matrix
*     CALL MNEMAT (SIGCOV, NSIG)
 
* Clear rest if covariance matrix and error vector (should these be considered
*   derived quantities?).
      DO 90    ISIG = NFREE + 1, NSIG
         DO 80    JSIG = 1, NSIG
            SIGCOV (ISIG, JSIG) = 0.
   80    CONTINUE
   90 CONTINUE
      DO 110   ISIG = NFREE + 1, NSIG
         DO 100   JSIG = 1, NSIG
            SIGCOV (JSIG, ISIG) = 0.
  100    CONTINUE
  110 CONTINUE
 
      GO TO 130
 
* Errors.
  120 CONTINUE
      WRITE (CHQMES, '(''Error'', I3, '' in MINUIT'')') IHQERR
      CALL HBUG (CHQMES, 'HQLSQC', IDMQ)
 
  130 CONTINUE
 
*Restore IMQFUN.
      IF (JMQFUN .GT. 0) IMQFUN = JMQFUN
 
      END
 
