*
* $Id: hbnsum.F,v 1.1.1.1 1996/01/16 17:07:53 mclareni Exp $
*
* $Log: hbnsum.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:53  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 23/10/92  11.51.54  by  R. J. Genik II
*-- Author :    R. J. Genik II   23/10/92
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION FUNCTION HBNSUM(NSTRT,NEND,NMAX,P,QQQ,ACCUR)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL FUNCTION HBNSUM(NSTRT,NEND,NMAX,P,QQQ,ACCUR)
#endif
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CALCULATE BINOMIAL DISTRIBUTION INTERVAL SUM
C-
C-   Returned value  : (DOUBLE PRECISION) SUM OF BINOMIAL TERMS
C-                     BETWEEN NSTRT, NEND TO ACCUR ORDER
C-   Inputs  : NSTRT,NEND : INTEGER; P,QQQ: DBLE PREC; ACCUR : REAL
C-   Outputs : NONE
C-   Controls: ACCUR ; Stops summing if rest of sum is smaller than
C-             requested accuracy.
C-
C-   CREATED  22-OCT-1992   R. J. Genik II
C----------------------------------------------------------------------
C
C     Local and passed variable declarations
C
C----------------------------------------------------------------------
C
      INTEGER NSTRT,NEND,NSTEP,NMAX,N,K
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION HLBINO, P,QQQ,DLNTRM,DLAST,DLNP,DLNQQQ
#endif
      REAL SMLNUM, ACCUR
C
#include "hbook/hcdifb.inc"
C----------------------------------------------------------------------
C
C
C     Small Num is used to terminate summing and increase speed by
C     not calculating terms which are not significant to the user.
C     This is how it works (it emulates a DO WHILE we care loop):
C
C           The summation extends from the most to least significant
C           terms (low to high for U, high to low for L). When the rest
C           of the sum (tail) becomes less than the requested accuracy,
C           the loop is terminated with a GOTO. The general procedure
C           is to calculate the log of the term, the check if we can
C           exponentiate it within machine error.
C
C           The user is responsible for putting NSTRT,NEND in the right
C           order. Mistakes will just waste time, not give the wrong
C           answer (because we have included a flag to make sure that
C           the terms are decreasing before we quit summing.)
C
C           When we are asked to only calculate one term, ACCUR is
C           set to its default value. The default value is the
C           smallest allowed by underflow. (i.e. maximum accuracy)
C
C----------------------------------------------------------------------
C
C
      HBNSUM=0.0
C
C
C----------------------------------------------------------------------
C             * Check if invalid values were sent
C----------------------------------------------------------------------
C
      IF ((NSTRT.LT.0).OR.(NEND.LT.0).OR.(NMAX.LT.0).OR.
     +  (QQQ.LE.0.).OR.(P.LE.0.)) GOTO 999
C
C
C
C----------------------------------------------------------------------
C             * Set accuracy, default to -lnbigp
C----------------------------------------------------------------------
C
      IF ((ACCUR.LE.0.).OR.(NSTRT.EQ.NEND).OR.
     +  (LOG(ACCUR).LT.(-LNBIGP-10.*ABS(NSTRT-NEND)))) THEN
        SMLNUM = -LNBIGP
      ELSE
        SMLNUM = LOG(ACCUR/(ABS(NSTRT-NEND)*10.))
      ENDIF
C
      DLNP = LOG(P)
      DLNQQQ = LOG(QQQ)
      N = NMAX
C
C
C----------------------------------------------------------------------
C             * Get the log of the first term for the first comparison
C----------------------------------------------------------------------
C
      DLAST = HLBINO(N,NSTRT) + NSTRT*DLNP+(N-NSTRT)*DLNQQQ
C
C
C
C----------------------------------------------------------------------
C             * Set step control for do loop
C----------------------------------------------------------------------
C
      NSTEP = 1
      IF (NSTRT.GT.NEND) NSTEP = -1
C
      DO 10 K=NSTRT,NEND,NSTEP
C
C
C----------------------------------------------------------------------
C             * Use LOG terms. For our purposes, the accuracy of
C               calculating p**k and the like is similiar to that
C               of k*log(p)
C----------------------------------------------------------------------
C
        DLNTRM = HLBINO(N,K) + K*DLNP+(N-K)*DLNQQQ
C
C----------------------------------------------------------------------
C             * If exp(dtemp) is within accuracy requested, or the
C               default of underflow protection, add to sum.
C----------------------------------------------------------------------
C
        IF (DLNTRM.GT.SMLNUM) THEN
          HBNSUM =HBNSUM + EXP(DLNTRM)
          DLAST = DLNTRM
C
C----------------------------------------------------------------------
C                   Terminate DO loops to save time once we acheive
C                   requested accuracy.
C                   BUT...
C                   Make sure we are not increasing before we
C                   terminate loop.
C----------------------------------------------------------------------
C
        ELSEIF (DLNTRM.LT.DLAST) THEN
          GOTO 999
C
C
        ENDIF
   10 CONTINUE
  999 RETURN
      END
