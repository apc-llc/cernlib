*
* $Id: hqin.F,v 1.1.1.1 1996/01/16 17:08:03 mclareni Exp $
*
* $Log: hqin.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:03  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/02 15/12/92  21.48.30  by  Rene Brun
*-- Author :
      SUBROUTINE HQIN (LHQIN)
      INTEGER LHQIN
* Reads multiquadric parameters from unit LHQIN
*!!!!! Warning...
* The pair of routines - HQIN, HQOUT - must be used with care.  In particular
*   HQIN recreates the LHQUAD bank structure, at least partially.  Moreover,
*   only the HQD... functions will have access to the paramaters (the HQF...
*   routines pick them up from HBOOK LHFIT banks).  Finally, after you have
*   finished using the parameters, CALL HQEND to drop the banks.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcqcor.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcunit.inc"
 
      INTEGER I, II, J, L, NWW, IHQERR
 
#if defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 2)
      DOUBLE PRECISION SS
#endif
#if !defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 1)
      REAL SS
#endif
 
      READ (LHQIN, '(8I10)') NSIG, NDIM, IMQFUN,
     +(NBINS (I), I = 1, NDIM)
      WRITE (LOUT, 10000) NSIG
 
* Allocate working space needed....
      CALL HQINIT (NSIG, NDIM, NBINS, IHQERR)
      IF (IHQERR. NE. 0) GO TO 60
 
* Lift LHQCOV bank, etc...
      CALL HQLIF2 (IHQERR)
      IF (IHQERR .NE. 0) GO TO 60
 
      READ (LHQIN, '(4G20.13)') VCONST, HQFMIN, HQFMAX
      WRITE (LOUT, 10100) HQFMIN, HQFMAX
 
      READ (LHQIN, '(4G20.13)') (SIGVMI (I), SIGVMA (I), I = 1, NDIM)
      DO 10    I = 1, NDIM
         SIGVT (I) = SIGVMA (I) - SIGVMI (I)
   10 CONTINUE
 
      DO 20    I = 1, NSIG
         READ (LHQIN, '(I5, 3G20.13 / (5X, 3G20.13))')
     +   II, SIGA (I), (SIGV (I, J), J = 1, NDIM), SIGDEL (I)
   20 CONTINUE
 
* Covariances.
* (Allow for fact that LHQCOV data has to start on 8-byte boundary.)
      DO 40    I = 1, NSIG
         DO 30    J = 1, I
            READ (LHQIN, '(G20.13)', END = 50) SS
            L = LHQCOV + MOD (LHQCOV, NWW)
            CALL UCOPY (SS,
     +      Q (L + NWW * ((J - 1) * NSIG + I - 1) + 1), NWW)
            CALL UCOPY (SS,
     +      Q (L + NWW * ((I - 1) * NSIG + J - 1) + 1), NWW)
   30    CONTINUE
   40 CONTINUE
 
      GO TO 70
 
   50 CONTINUE
      WRITE (LOUT, '(1X, ''No covariances.'')')
      GO TO 70
 
   60 CONTINUE
      CALL HBUG('Not enough space in /PAWC/.', 'HQIN', 0)
      GO TO 70
 
   70 CONTINUE
 
10000 FORMAT (1X, 'Reading ', I5, ' multiquadric parameters.')
10100 FORMAT (1X, 'Function range', G12.5, ' to', G12.5)
 
      END
