*
* $Id: hqout.F,v 1.1.1.1 1996/01/16 17:08:05 mclareni Exp $
*
* $Log: hqout.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:05  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 22/11/92  15.16.56  by  John Allison
*-- Author :
      SUBROUTINE HQOUT (LHQOUT, ICOVAR)
      INTEGER LHQOUT, ICOVAR
* Writes multiquadric parameters to unit LHQOUT
* ICOVAR = 0, no covariance matrix elements.
* ICOVAR = 1, write covariance matrix elements.
*!!!!! Use with care!  See warning in HQIN header.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcqcor.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcunit.inc"
 
      INTEGER I, J, L, NWW
 
#if defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 2)
      DOUBLE PRECISION SS
#endif
#if !defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 1)
      REAL SS
#endif
 
      WRITE (LOUT, 10000) NSIG
 
      WRITE (LHQOUT, '(8I10)') NSIG, NDIM, IMQFUN,
     +(NBINS (I), I = 1, NDIM)
 
      WRITE (LHQOUT, '(4G20.13)') VCONST, HQFMIN, HQFMAX
 
      WRITE (LHQOUT, '(4G20.13)') (SIGVMI (I), SIGVMA (I), I = 1, NDIM)
 
      DO 10    I = 1, NSIG
         WRITE (LHQOUT, '(I5, 3G20.13 / (5X, 3G20.13))')
     +   I, SIGA (I), (SIGV (I, J), J = 1, NDIM), SIGDEL (I)
   10 CONTINUE
 
* Covariances.
* (Allow for fact that LHQCOV data has been made to start on 8-byte boundary.)
      IF (ICOVAR .EQ. 1) THEN
         L = LHQCOV + MOD (LHQCOV, NWW)
         DO 30    I = 1, NSIG
            DO 20    J = 1, I
               CALL UCOPY (Q (L + NWW * ((J - 1) * NSIG + I - 1) + 1),
     +         SS, NWW)
               WRITE (LHQOUT, '(G20.13)') SS
   20       CONTINUE
   30    CONTINUE
      END IF
 
10000 FORMAT (1X, 'Writing ', I5, ' multiquadric parameters.')
 
      END
