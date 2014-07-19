*
* $Id: hqwrif.F,v 1.1.1.1 1996/01/16 17:08:07 mclareni Exp $
*
* $Log: hqwrif.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:07  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/00 23/10/93  17.10.41  by  Rene Brun
*-- Author :    John Allison   18/12/92
      SUBROUTINE HQWRIF (LUN)
*
* Write Fortran77 function to unit LUN.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcqcor.inc"
#include "hbook/hcbook.inc"
 
      WRITE (LUN, 10000) NDIM, NDIM, NDIM, NSIG, NDIM, NSIG, NSIG
      WRITE (LUN, 10100) NSIG, NDIM, IMQFUN
      WRITE (LUN, 10200) VCONST
      IF (NDIM .EQ.1) THEN
         WRITE (LUN, 10300) (SIGVMI (I), I = 1, NDIM)
         WRITE (LUN, 10400) (SIGVT (I), I = 1, NDIM)
      ELSE
         WRITE (LUN, 10500) (SIGVMI (I), I = 1, NDIM)
         WRITE (LUN, 10600)
         WRITE (LUN, 10700) (SIGVT (I), I = 1, NDIM)
         WRITE (LUN, 10600)
      END IF
      WRITE (LUN, 10800) ((SIGV (J, I), J = 1, NSIG), I = 1, NDIM)
      WRITE (LUN, 10600)
      WRITE (LUN, 10900) (SIGDEL (I), I = 1, NSIG)
      WRITE (LUN, 10600)
      WRITE (LUN, 11000) (SIGA (I), I = 1, NSIG)
      WRITE (LUN, 10600)
      WRITE (LUN, 11100)
      WRITE (LUN, 11200)
      WRITE (LUN, 11300)
 
10000 FORMAT (
#if defined(CERNLIB_DOUBLE)
     +6X, 'DOUBLE PRECISION FUNCTION HQUADF(V)'/
     +6X, 'DOUBLE PRECISION V(', I2, ')'/
     +6X, 'INTEGER NPAR, NDIM, IMQFUN, I, J'/
     +6X, 'DOUBLE PRECISION HQDJ, VV, VCONST'/
     +6X, 'DOUBLE PRECISION SIGVMI(', I2, '), SIGVT(', I2, ')'/
     +6X, 'DOUBLE PRECISION SIGV(', I4, ',', I2, ')'/
     +6X, 'DOUBLE PRECISION SIGDEL(', I4, ')'/
     +6X, 'DOUBLE PRECISION SIGA(', I4, ')')
#endif
#if !defined(CERNLIB_DOUBLE)
     +6X, 'REAL FUNCTION HQUADF(V)'/
     +6X, 'REAL V(', I2, ')'/
     +6X, 'INTEGER NPAR, NDIM, IMQFUN, I, J'/
     +6X, 'REAL HQDJ, VV, VCONST'/
     +6X, 'REAL SIGVMI(', I2, '), SIGVT(', I2, ')'/
     +6X, 'REAL SIGV(', I4, ',', I2, ')'/
     +6X, 'REAL SIGDEL(', I4, ')'/
     +6X, 'REAL SIGA(', I4, ')')
#endif
10100 FORMAT (
     +6X, 'DATA NPAR, NDIM, IMQFUN /', I5, ',', I5, ',', I5, '/')
10200 FORMAT (6X, 'DATA VCONST /', G20.13, '/')
10300 FORMAT (6X, 'DATA SIGVMI /', G20.13, '/')
10400 FORMAT (6X, 'DATA SIGVT /', G20.13, '/')
10500 FORMAT (6X, 'DATA SIGVMI /', G20.13/ (5X, '+,', G20.13))
10600 FORMAT (5X, '+/')
10700 FORMAT (6X, 'DATA SIGVT /', G20.13/ (5X, '+,', G20.13))
10800 FORMAT (6X, 'DATA SIGV /', G20.13/ (5X, '+,', G20.13))
10900 FORMAT (6X, 'DATA SIGDEL /', G20.13/ (5X, '+,', G20.13))
11000 FORMAT (6X, 'DATA SIGA /', G20.13/ (5X, '+,', G20.13))
11100 FORMAT (
     +6X, 'HQUADF = 0.'/
     +6X, 'DO 20 J = 1, NPAR'/
     +9X, 'HQDJ = 0.'/
     +9X, 'DO 10 I = 1, NDIM'/
     +12X, 'VV = (V (I) - SIGVMI (I)) / SIGVT (I)'/
     +12X, 'HQDJ = HQDJ + (VV - SIGV (J, I)) ** 2'/
     +3X, '10 ', 3X, 'CONTINUE'/
     +9X, 'HQDJ = HQDJ + SIGDEL (J) ** 2'/
     +9X, 'HQDJ = SQRT (HQDJ)'/
     +9X, 'HQUADF = HQUADF + SIGA (J) * HQDJ'/
     +3X, '20 CONTINUE')
11200 FORMAT (
     +6X, 'IF (IMQFUN .EQ. 2) HQUADF = VCONST * EXP (HQUADF)')
11300 FORMAT (6X, 'END')
 
      END
