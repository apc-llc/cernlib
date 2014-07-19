*
* $Id: e406m.F,v 1.1.1.1 1996/04/01 15:01:26 mclareni Exp $
*
* $Log: e406m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:26  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE E406M
#include "iorc.inc"
C     Routine to test the MATHLIB routines CHECF and DCHECF (E406)
C     The coefficients of the Chebyshev series are calculated, and the
C     series is evaluated at three points and compared with the value of
C     the function they approximate.         T Hepworth, May 16th 1990

C     Specify the number of tests
      PARAMETER ( NT=3 )
#include "gen/def64.inc"
     +      A(NT),B(NT),EPS(NT),C(0:128),DELTA,X(3),
     +      SOL,EXACT,ERROR,ERRMAX,TSTERR,
     +      E406F1,E406F2,E406F3,E406F4
      EXTERNAL         E406F2,E406F3,E406F4

C     Specify maximum absolute error permitted for a successful test
      PARAMETER ( TSTERR=1D-11 )

C     The test parameters
      DATA A(1),B(1),EPS(1) / -1D0, 1D0, 1D-16 /
      DATA A(2),B(2),EPS(2) / -1D0, 1D0, 1D-16 /
      DATA A(3),B(3),EPS(3) / -1D0, 1D0, 1D-16 /

      CALL HEADER('E406',0)
C     Initialise largest error term
      ERRMAX=0D0
      DO 100 I=1,NT
         WRITE(LOUT,'(/'' Test Number'',I3)') I
C        Calculate coefficients of Chebyshev series
         IF (I .EQ. 1) THEN
#if defined(CERNLIB_DOUBLE)
            CALL DCHECF(E406F2,A(I),B(I),EPS(I),C,N,DELTA)
         ELSEIF (I .EQ. 2) THEN
            CALL DCHECF(E406F3,A(I),B(I),EPS(I),C,N,DELTA)
         ELSE
            CALL DCHECF(E406F4,A(I),B(I),EPS(I),C,N,DELTA)
#endif
#if !defined(CERNLIB_DOUBLE)
            CALL  CHECF(E406F2,A(I),B(I),EPS(I),C,N,DELTA)
         ELSEIF (I .EQ. 2) THEN
            CALL  CHECF(E406F3,A(I),B(I),EPS(I),C,N,DELTA)
         ELSE
            CALL  CHECF(E406F4,A(I),B(I),EPS(I),C,N,DELTA)
#endif
         ENDIF
C        Set up positions at which to evaluate the functions
         X(1)=A(I)
         X(2)=0.5D0*( A(I)+B(I) )
         X(3)=B(I)

C        Evaluate sum of calculated series at points X1,X2,X3
         DO 50 J=1,3
            SOL=E406F1(C,N,X(J))
            IF (I .EQ. 1) THEN
               EXACT=E406F2(X(J))
            ELSEIF (I .EQ. 2) THEN
               EXACT=E406F3(X(J))
            ELSE
               EXACT=E406F4(X(J))
            ENDIF
C           Calculate Absolute Error
            ERROR= ABS( EXACT-SOL)
            WRITE(LOUT,'('' X='',F10.4,6X,''No Coefficients'',I4,6X,
     +                      ''Delta'',F25.16)') X(J),N,DELTA
            WRITE(LOUT,'('' Calculated Sum   '',F25.16)') SOL
            WRITE(LOUT,'('' Function Value   '',F25.16)') EXACT
            WRITE(LOUT,'('' Absolute Error   '',F25.16)') ERROR
            ERRMAX=MAX( ERRMAX,ERROR )
 50      CONTINUE
100   CONTINUE
      WRITE(LOUT,'(/'' Largest Error was'',1P,D10.1)') ERRMAX
C     Check if the test was successful
      IRC=ITEST('E406',ERRMAX .LE. TSTERR)
      CALL PAGEND('E406')
      RETURN
      END

