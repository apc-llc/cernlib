*
* $Id: hqputf.F,v 1.1.1.1 1996/01/16 17:08:05 mclareni Exp $
*
* $Log: hqputf.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:05  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 22/11/92  12.13.37  by  John Allison
*-- Author :    John Allison   12/09/92
      SUBROUTINE HQPUTF (LFIT)
      INTEGER LFIT
* Take multiquadric parameters from /HCQCOM/ and place in bank 'HFIT'.
* See HSUPIS for banks definitions.
 
#include "hbook/hcbook.inc"
#include "hbook/hcqcom.inc"
#include "hbook/hcqcor.inc"
 
      CHARACTER*4 NAME
      INTEGER I, II, JJ, L, NBLOCK, NBLOJJ
      INTEGER LFCOV, LCOV
      INTEGER NWW
#if defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 2)
      DOUBLE PRECISION SS
#endif
#if !defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 1)
      REAL SS
#endif
 
      IF (LFIT .LE. 0) RETURN
      LFCOV = 0
      L = LQ (LFIT)
   10 CONTINUE
      IF (L. NE. 0)THEN
         CALL UHTOC (IQ (L - 4), 4, NAME, 4)
         IF (NAME .EQ. 'HFCO')THEN
            LFCOV = L
         ELSE
            GO TO 10
         END IF
      END IF
 
      Q (LFIT + 7) = HQFMIN
      Q (LFIT + 8) = HQFMAX
 
      NBLOCK = NWW * NSIG
 
* LFIT bank: NSIG fitted parameters,
*            5 special parameters,
*            (NDIM + 1) * NSIG fixed parameters,
      II = 11
      CALL UCOPY (SIGA, Q (LFIT + II), NBLOCK)
      II = II + NBLOCK
      SS = IMQFUN
      CALL UCOPY (SS, Q (LFIT + II), NWW)
      II = II + NWW
      SS = VCONST
      CALL UCOPY (SS, Q (LFIT + II), NWW)
      II = II + 4 *NWW
      DO 20    I = 1, NSIG
         SS = SIGV (I, 1)
         CALL UCOPY (SS, Q (LFIT + II), NWW)
         II = II + NWW
   20 CONTINUE
      IF (NDIM .GE. 2) THEN
         DO 30    I = 1, NSIG
            SS = SIGV (I, 2)
            CALL UCOPY (SS, Q (LFIT + II), NWW)
            II = II + NWW
   30    CONTINUE
      END IF
      IF (NDIM .GE. 3) THEN
         DO 40    I = 1, NSIG
            SS = SIGV (I, 3)
            CALL UCOPY (SS, Q (LFIT + II), NWW)
            II = II + NWW
   40    CONTINUE
      END IF
      IF (NDIM .GE. 4) THEN
         CALL HBUG ('Not programmed for >=4 dimensions.', 'HQPUTF',
     +   IDMQ)
      END IF
      DO 50    I = 1, NSIG
         SS = SIGDEL (I)
         CALL UCOPY (SS, Q (LFIT + II), NWW)
         II = II + NWW
   50 CONTINUE
 
* Lower triangle of covariance bank (if it exists).
* (Allow for fact that LHQCOV data has been made to start on 8-byte boundary.)
      IF (LFCOV .GT. 0) THEN
         LCOV = LHQCOV + MOD (LHQCOV, NWW)
         II = 1
         JJ = 1
         NBLOJJ = NWW * NSIG
         DO 60    I = 1, NSIG
            NBLOCK = NWW * I
            CALL UCOPY (Q (LCOV + JJ), Q (LFCOV + II), NBLOCK)
            II = II + NBLOCK
            JJ = JJ + NBLOJJ
   60    CONTINUE
      END IF
 
      END
