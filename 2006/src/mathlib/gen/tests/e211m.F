*
* $Id: e211m.F,v 1.1.1.1 1996/04/01 15:01:26 mclareni Exp $
*
* $Log: e211m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:26  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE E211M
#include "iorc.inc"
C     Test of MATHLIB routines RCSPLN,DCSPLN,RCSPNT,DCSPNT (E211)
C     Five individual tests are given: Tests numbers 1 and 2 test the
C     spline fit parameters for the simple cases N=2 and N=3. The
C     expected values can be calculated easily by hand. For ease of
C     calculation, a unitary interval width was taken (ie H=1)
C     Written by T Hepworth, CN division, Cern, Meyrin
      INTEGER    DMAX
C     Specify total number of tests
      PARAMETER ( NT=5,DMAX=120,NDIM=120 )
#include "gen/def64.inc"
     +   X(0:DMAX),Y(0:NDIM,1),START(NT),FINISH(NT),
     +   A(NDIM,NT),B(NDIM,NT),C(NDIM,NT),D(NDIM,NT),H,
     +   SOL(2),EXACT(2),U,V,NUMTOL,ERROR,ERRMAX,TSTERR,
     +   FITPAR(4,3),FITXCT(4,3),ALPHA1,ALPHA2
      INTEGER          N(NT),M(NT),TESTNO

C     Set numerical tolerance for comparison with zero
      PARAMETER ( NUMTOL=1D-10 )
C     Set largest relative error permitted for a successful test
      PARAMETER ( TSTERR=1D-5)
C     Set test parameters, first two test spline-fit parameters, the
C     others test the integrals of the spline function
      DATA START(1),FINISH(1),N(1),M(1) / 1D0, 3D0,   2,1/
      DATA START(2),FINISH(2),N(2),M(2) /-1D0, 2D0,   3,1/
      DATA START(3),FINISH(3),N(3),M(3) / 0D0, 3D0,NDIM,1/
      DATA START(4),FINISH(4),N(4),M(4) / 0D0, 3D0,NDIM,1/
      DATA START(5),FINISH(5),N(5),M(5) /-2D0, 2D0,NDIM,1/

      CALL HEADER('E211',0)
      WRITE(LOUT,'(/20X,''Testing the calculated spline-fit '',
     +                                         ''parameters'')')
C     Initialise largest recorded error
      ERRMAX=0D0
      DO 200 TESTNO=1,2
         WRITE(LOUT,'(/'' Test Number '',I2)') TESTNO
         H= ( FINISH(TESTNO)-START(TESTNO) )/ N(TESTNO)
C        NOTE here H should be 1
C        Set up the X nodes as being evenly spaced out
         DO 100 I=0,N(TESTNO)
            X(I)=START(TESTNO)+(I*H)
            IF (TESTNO .EQ. 1) Y(I,1)=EXP(-X(I))
            IF (TESTNO .EQ. 2) Y(I,1)= X(I)*X(I)
100      CONTINUE

         DO 180 MODE=0,1
            IF (MODE .EQ. 0) WRITE(LOUT,'(/'' MODE=0:Natural Spline'')')
            IF (MODE .EQ. 1) WRITE(LOUT,'(/'' MODE=1:'')')
C           Calculate the spline-fit parameters
#if defined(CERNLIB_DOUBLE)
            CALL DCSPLN(N(TESTNO),X,M(TESTNO),Y,NDIM,MODE,A,B,C,D)
#endif
#if !defined(CERNLIB_DOUBLE)
            CALL RCSPLN(N(TESTNO),X,M(TESTNO),Y,NDIM,MODE,A,B,C,D)
#endif
C           Store the fit parameters in another array for ease of
C           manipulation (these are the calculated parameters)
            DO 120 I=1,N(TESTNO)
               FITPAR(1,I)=A(I,1)
               FITPAR(2,I)=B(I,1)
               FITPAR(3,I)=C(I,1)
               FITPAR(4,I)=D(I,1)
120         CONTINUE

C           Calculate the expected values of the fit parameters (these
C           are stored in FITXCT
            DO 140 I=1,N(TESTNO)
               FITXCT(1,I)=Y(I-1,1)
140         CONTINUE
            ALPHA1=(Y(2,1)-(2*Y(1,1))+Y(0,1))
            ALPHA2=(Y(3,1)-(2*Y(2,1))+Y(1,1))
            IF (TESTNO .EQ. 1 .AND. MODE .EQ. 0) THEN
               FITXCT(3,1)= 0D0
               FITXCT(3,2)= 3*ALPHA1/4
               FITXCT(4,1)= ALPHA1/4
               FITXCT(4,2)= -ALPHA1/4
            ENDIF
            IF (TESTNO .EQ. 1 .AND. MODE .EQ. 1) THEN
               FITXCT(3,1)= ALPHA1/2
               FITXCT(3,2)= FITXCT(3,1)
               FITXCT(4,1)= 0D0
               FITXCT(4,2)= 0D0
            ENDIF
            IF (TESTNO .EQ. 1) THEN
               FITXCT(2,1)=Y(1,1)-Y(0,1)-FITXCT(3,1)-FITXCT(4,1)
               FITXCT(2,2)=Y(2,1)-Y(1,1)-FITXCT(3,2)-FITXCT(4,2)
            ENDIF
            IF (TESTNO .EQ. 2 .AND. MODE .EQ. 0) THEN
               FITXCT(3,1)= 0D0
               FITXCT(3,2)= (4*ALPHA1-ALPHA2)/5
               FITXCT(3,3)= (4*ALPHA2-ALPHA1)/5
               FITXCT(4,1)= (4*ALPHA1-ALPHA2)/15
               FITXCT(4,2)= (ALPHA2-ALPHA1)/3
               FITXCT(4,3)= (ALPHA1-4*ALPHA2)/15
            ENDIF
            IF (TESTNO .EQ. 2 .AND. MODE .EQ. 1) THEN
               FITXCT(3,1)= (5*ALPHA1-ALPHA2)/8
               FITXCT(3,2)= FITXCT(3,1)
               FITXCT(3,3)= (5*ALPHA2-ALPHA1)/8
               FITXCT(4,1)= 0D0
               FITXCT(4,2)= (ALPHA2-ALPHA1)/4
               FITXCT(4,3)= 0D0
            ENDIF
            IF (TESTNO .EQ. 2) THEN
               FITXCT(2,1)= Y(1,1)-Y(0,1)-FITXCT(3,1)-FITXCT(4,1)
               FITXCT(2,2)= Y(2,1)-Y(1,1)-FITXCT(3,2)-FITXCT(4,2)
               FITXCT(2,3)= Y(3,1)-Y(2,1)-FITXCT(3,3)-FITXCT(4,3)
            ENDIF
            WRITE(LOUT,'('' Param'',15X,''Calculated'',20X,''Exact'',
     +                    8X,''Relative Error'')')
            DO 160 I=1,4
               DO 150 J=1,N(TESTNO)
C                 Calculate the relative error
                  ERROR=ABS( FITXCT(I,J)-FITPAR(I,J) )
                  IF (ABS(FITPAR(I,J)) .GE. NUMTOL)
     +                    ERROR=ERROR/ABS(FITPAR(I,J))
                  ERRMAX= MAX( ERRMAX,ERROR )
                  WRITE(LOUT,'('' ('',I1,'','',I1,'')'',2F25.16,
     +              1P,D10.1)') I,J,FITPAR(I,J),FITXCT(I,J),ERROR
150            CONTINUE
160         CONTINUE
180      CONTINUE
200   CONTINUE
C     ****** End of tests one and two that tested the spline-fit ******


      WRITE(LOUT,'(/25X,''Testing the spline integrals'')')
C     The following tests, check the accuracy of the integrals computed
C     by the routines RCSPNT and DCSPNT, by comparing them with the
C     analytic integral of the function to which a spline is fit.
      DO 500 TESTNO=3,NT
         WRITE(LOUT,'(/'' Test Number'',I3)') TESTNO
         H= ( FINISH(TESTNO)-START(TESTNO) )/ N(TESTNO)
         DO 300 I=0,N(TESTNO)
            X(I)=START(TESTNO)+(I*H)
            IF (TESTNO .EQ. 3) Y(I,1)= EXP(-X(I))
            IF (TESTNO .EQ. 4) Y(I,1)= (X(I)**7)-(6D0*X(I)**4)
            IF (TESTNO .EQ. 5) Y(I,1)= SIN( X(I) )
300      CONTINUE

         DO 400 MODE=0,1
            IF (MODE .EQ. 0) WRITE(LOUT,'(/'' MODE=0:Natural Spline'')')
            IF (MODE .EQ. 1) WRITE(LOUT,'(/'' MODE=1:'')')
C           Calculate the integrals of the spline function
#if defined(CERNLIB_DOUBLE)
            CALL DCSPNT(N(TESTNO),X,M(TESTNO),Y,NDIM,MODE,A,B,C,D)
#endif
#if !defined(CERNLIB_DOUBLE)
            CALL RCSPNT(N(TESTNO),X,M(TESTNO),Y,NDIM,MODE,A,B,C,D)
#endif
C           Store the integrals in SOL, SOL(2)=double integral
            SOL(1)= A(N(TESTNO),1)
            SOL(2)= B(N(TESTNO),1)
            U= START(TESTNO)
            V= FINISH(TESTNO)

C           Compute the integral of the function being approximated
            IF (TESTNO .EQ. 3) THEN
               EXACT(1)=EXP(-U)-EXP(-V)
               EXACT(2)=EXP(-V)+(EXP(-U)*(V-U-1D0))
            ELSEIF (TESTNO .EQ. 4) THEN
               EXACT(1)=((V**8-U**8)/8D0)-(1.2D0*(V**5-U**5))
               EXACT(2)=(V**9/72)-(V**6/5)+( V*(6*U**5/5-U**8/8) )-
     +                     (U**6)+(U**9/9)
            ELSE
               EXACT(1)= COS(U)-COS(V)
               EXACT(2)= (V-U)*COS(U)+SIN(U)-SIN(V)
            ENDIF

C           Compute the relative errors of the integrals
            DO 320 J=1,2
               ERROR= ABS( SOL(J)-EXACT(J) )
               IF (ABS(SOL(J)) .GE. NUMTOL) ERROR=ERROR/ABS(SOL(J))
               IF (J .EQ. 1) WRITE(LOUT,'('' Single integral of'',
     +             '' spline function'',5X,F25.16)') SOL(J)
               IF (J .EQ. 2) WRITE(LOUT,'('' Double integral of'',
     +             '' spline function'',5X,F25.16)') SOL(J)
               WRITE(LOUT,'('' Integral of function being '',
     +                       ''approximated'',F25.16)') EXACT(J)
               WRITE(LOUT,'('' Relative Error'',25X,1P,D10.1)') ERROR
               ERRMAX=MAX( ERRMAX,ERROR )
320         CONTINUE
400       CONTINUE
500   CONTINUE
      WRITE(LOUT,'(/'' Largest Error was'',1P,D10.1 )') ERRMAX
      IRC=ITEST('E211',ERRMAX .LE. TSTERR)
      CALL PAGEND('E211')
      RETURN
      END
