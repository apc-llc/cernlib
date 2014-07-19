*
* $Id: c324m.F,v 1.1.1.1 1996/04/01 15:01:16 mclareni Exp $
*
* $Log: c324m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:16  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C324M
C     This program tests the MATHLIB routines ATANI and DATANI (C324)
#include "gen/def64.inc"
     +      DATANI
#include "gen/def64.inc"
     +      X(7),EXACT(7),RESULT(7),ERROR(7),ERRMAX,TSTERR
      REAL  ATANI,SX(7),SEXACT(7),SRESULT(7),SERROR(7),SERRMAX,STSTERR
C     Set the maximum error allowed for the test to still be considered
C     successful
      PARAMETER ( TSTERR=1D-13 )
      PARAMETER (STSTERR=1E-6  )
      LOGICAL LTEST
#include "iorc.inc"
C     Set up test data and theoretical solutions
      DATA X(1),EXACT(1) / -6.8D0, -3.1578031718126307D+00 /
      DATA X(2),EXACT(2) / -2.0D0, -1.5760154034463227D+00 /
      DATA X(3),EXACT(3) / -0.8D0, -0.7531060909241987D+00 /
      DATA X(4),EXACT(4) /  0.0D0, -0.0D+00                /
      DATA X(5),EXACT(5) /  0.8D0,  0.7531060909241987D+00 /
      DATA X(6),EXACT(6) /  2.0D0,  1.5760154034463227D+00 /
      DATA X(7),EXACT(7) /  6.8D0,  3.1578031718126307D+00 /

      CALL HEADER('C324',0)
C     Initialise maximum error
      ERRMAX=0D0
      SERRMAX=0E0
      LTEST=.TRUE.
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/5X,''X'',8X,''DATANI/ATANI'',13X,
     +        ''Exact Value'',10x,''Error'')')
#endif
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/5X,''X'',8X,''ATANI'',13X,
     +        ''Exact Value'',10x,''Error'')')
#endif
      DO 100 I=1,7
      SX(I)=X(I)
      SEXACT(I)=EXACT(I)
#if defined(CERNLIB_DOUBLE)
         RESULT(I)=DATANI(X(I))
        SRESULT(I)= ATANI(SX(I))
#endif
#if !defined(CERNLIB_DOUBLE)
         RESULT(I)=ATANI(X(I))
#endif
C        Calculate absolute errors
         ERROR(I)= ABS ( RESULT(I)-EXACT(I) )
         ERRMAX=MAX ( ERRMAX,ERROR(I) )
         LTEST=LTEST .AND. ERRMAX .LE. TSTERR
      WRITE(LOUT,'(F6.1,2F24.16,1P,D10.1)') X(I),RESULT(I),EXACT(I),
     + ERROR(I)
#if defined(CERNLIB_DOUBLE)
        SERROR(I)= ABS (SRESULT(I)-SEXACT(I) )
        SERRMAX=MAX ( SERRMAX,SERROR(I) )
         LTEST=LTEST .AND. SERRMAX .LE. STSTERR
      WRITE(LOUT,'(6X,2F24.8,1P,D10.1)') SRESULT(I),SEXACT(I),SERROR(I)
#endif
100   CONTINUE
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/'' Largest Error'',1P,D10.1)') ERRMAX
#endif
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/''Double Precision largest Error'',1P,D10.1)')ERRMAX
      WRITE(LOUT,'(''Single Precision largest Error'',1P,D10.1)')SERRMAX
#endif
      WRITE(LOUT,'(1X)')

C     Check if the test was successful
      IRC=ITEST('C324',LTEST)
      CALL PAGEND('C324')
      RETURN
      END
