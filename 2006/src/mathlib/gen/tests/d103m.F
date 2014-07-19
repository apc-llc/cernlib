*
* $Id: d103m.F,v 1.2 1996/11/25 16:17:20 cernlib Exp $
*
* $Log: d103m.F,v $
* Revision 1.2  1996/11/25 16:17:20  cernlib
* Protect the loop finding eps such that 1+eps!=1 against optimisation.
* Introduce a SUBROUTINE to do some of the work.
*
* Revision 1.1.1.1  1996/04/01 15:01:22  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D103M
C     This program tests the operation of MATHLIB subprograms
C     GAUSS, DGAUSS, and QGAUSS (D103)

      LOGICAL LTEST, LTEST1,LTEST2
      COMMON /D103LT1/LTEST1
#if (defined(CERNLIB_QUAD))&&(!defined(CERNLIB_IBMRS))
      COMMON /D103LT2/LTEST2
#endif
#include "iorc.inc"
      CALL HEADER('D103',0)
      LTEST=.TRUE.
      CALL D103D
        LTEST=LTEST .AND. LTEST1
#if (defined(CERNLIB_QUAD))&&(!defined(CERNLIB_IBMRS))
      CALL D103Q
        LTEST=LTEST .AND. LTEST2
#endif
      IRC=ITEST('D103',LTEST)
      CALL PAGEND('D103')
      RETURN
      END

C     DERIVED FROM KERNLIB TEST ESSENTIALLY FROM G.A. ERSKINE

      SUBROUTINE D103D

#include "gen/imp64.inc"
      REAL GAUSS,A1,B1,EPS1,D103F1,P1,Q1,EXACT1,APPRX1,ERR1,RNF,G1,G1S
      REAL RELPRT
      LOGICAL OKFN1,OKFN2,OKPK
      LOGICAL LTEST1
      EXTERNAL D103F1,D103F2

      COMMON /D103C1/ A1,B1,P1,Q1,RNF
      COMMON /D103C2/ A,B,P,Q,DNF
      COMMON /D103LT1/LTEST1
#include "iorc.inc"

      DIMENSION MRATIO(2),ERR1(10),ERR2(10),RELPRT(2)
      DIMENSION OKFN1(10),OKFN2(10)

C     Specify the largest relative error permitted in a successful test
      PARAMETER ( TSTERR= 1D-14 )
      PARAMETER (R0 = 0, R1 = 1, R2 = 2, HF = R1/2)

      DATA MRATIO /20,200/

      LTEST1=.TRUE.
      G1=1
   10 CALL REPSIL(G1,G1S)
      IF(G1S .NE. 1) GO TO 10
      RELPRT(1)=2*G1
      EPS1=MRATIO(1)*RELPRT(1)

#if defined(CERNLIB_DOUBLE)
      G2=1
   11 CALL DEPSIL(G2,G2S)
      IF(G2S .NE. 1) GO TO 11
      RELPRT(2)=2*G2
      EPS2=MRATIO(2)*RELPRT(2)
#endif
C     WRITE(LOUT,'(1X,2D12.4/)') RELPRT(1),RELPRT(2)

      PI = 3.14159 26535 89793D0
      WRITE(LOUT,'(1X)')
C
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(4X,''NFN'',12X,''EXACT'',19X,''DGAUSS'',15X,
     + ''GAUSS'')')
#endif
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(4X,''NFN'',12X,''EXACT'',19X,''GAUSS'')')
#endif
C  1. SHARP PEAK

      DNF=1
      RNF=1
      NFN=1
      A=HF
      B=R1/10
      P=100
      Q=R2/10
      EXACT=-SQRT(PI)
      A1=A
      B1=B
      P1=P
      Q1=Q
      EXACT1=EXACT
      APPRX1=GAUSS(D103F1,A1,B1,EPS1)
      ERR1(NFN)=ABS(APPRX1-EXACT1)
      OKFN1(NFN)=ERR1(NFN) .LT. EPS1
#if defined(CERNLIB_DOUBLE)
      APPRX2=DGAUSS(D103F2,A,B,EPS2)
      ERR2(NFN)=ABS(APPRX2-EXACT)
      OKFN2(NFN)=ERR2(NFN) .LT. EPS2
      WRITE(LOUT,'(1X,I5,2F25.15,F15.6/)') NFN,EXACT,APPRX2,APPRX1
#endif
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(1X,I5,2F25.15/)') NFN,EXACT,APPRX1
#endif
C
C  2. NON-POLYNOMIAL END-POINTS.

      DNF=2
      RNF=2
      NFN=2
      A=HF
      B=-3*HF
      P=1
      EXACT=SIGN(R1/8,B-A)*P*PI*(B-A)**2
      A1=A
      B1=B
      P1=P
      EXACT1=EXACT
      APPRX1=GAUSS(D103F1,A1,B1,EPS1)
      ERR1(NFN)=ABS(APPRX1-EXACT1)
      OKFN1(NFN)=ERR1(NFN) .LT. EPS1
#if defined(CERNLIB_DOUBLE)
      APPRX2=DGAUSS(D103F2,A,B,EPS2)
      ERR2(NFN)=ABS(APPRX2-EXACT)
      OKFN2(NFN)=ERR2(NFN) .LT. EPS2
      WRITE(LOUT,'(1X,I5,2F25.15,F15.6/)') NFN,EXACT,APPRX2,APPRX1
#endif
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(1X,I5,2F25.15/)') NFN,EXACT,APPRX1
#endif
C
C  3. OSCILLATORY FUNCTION (ABSOLUTE ERROR CRITERION).

      DNF=3
      RNF=3
      NFN=3
      A=0
      B=3*PI/2
      P=1
      Q=10
      EXACT=0
      A1=A
      B1=B
      P1=P
      Q1=Q
      EXACT1=EXACT
      APPRX1=GAUSS(D103F1,A1,B1,EPS1)
      ERR1(NFN)=ABS(APPRX1-EXACT1)
      OKFN1(NFN)=ERR1(NFN) .LT. EPS1
#if defined(CERNLIB_DOUBLE)
      APPRX2=DGAUSS(D103F2,A,B,EPS2)
      ERR2(NFN)=ABS(APPRX2-EXACT)
      OKFN2(NFN)=ERR2(NFN) .LT. EPS2
      WRITE(LOUT,'(1X,I5,2F25.15,F15.6/)') NFN,EXACT,APPRX2,APPRX1
#endif
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(1X,I5,2F25.15/)') NFN,EXACT,APPRX1
#endif
C
C  4. OSCILLATORY FUNCTION (RELATIVE ERROR CRITERION).

      DNF=4
      RNF=4
      NFN=4
      A=0
      B=-3*PI/2
      P=1D20
      Q=12
      EXACT=P*(B-A)
      A1=A
      B1=B
      P1=P
      Q1=Q
      EXACT1=EXACT
      APPRX1=GAUSS(D103F1,A1,B1,EPS1)
      ERR1(NFN)=ABS((APPRX1-EXACT1)/EXACT1)
      OKFN1(NFN)=ERR1(NFN) .LT. EPS1
#if defined(CERNLIB_DOUBLE)
      APPRX2=DGAUSS(D103F2,A,B,EPS2)
      ERR2(NFN)=ABS((APPRX2-EXACT)/EXACT)
      OKFN2(NFN)=ERR2(NFN) .LT. EPS2
      WRITE(LOUT,'(1X,I5,2E25.15,E15.6/)') NFN,EXACT,APPRX2,APPRX1
#endif
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(1X,I5,2E25.15/)') NFN,EXACT,APPRX1
#endif

      OKPK=.TRUE.
      DO 1 I=1,NFN
#if !defined(CERNLIB_DOUBLE)
    1 OKPK=OKPK .AND. OKFN1(I)
#endif
#if defined(CERNLIB_DOUBLE)
    1 OKPK=OKPK .AND. OKFN1(I) .AND. OKFN2(I)
#endif
      IF(.NOT.OKPK) THEN
       WRITE(LOUT,2000)
       DO 2 I=1,NFN
       IF(.NOT.OKFN1(I)) WRITE(LOUT,2001) I,ERR1(I),EPS1
#if defined(CERNLIB_DOUBLE)
       IF(.NOT.OKFN2(I)) WRITE(LOUT,2002) I,ERR2(I),EPS2
#endif
    2  CONTINUE
      ENDIF
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
      DNF=5
      RNF=5
      NFN=5
      A=1
      B=0
      A1=1
      B1=0
      APPRX1=GAUSS(D103F1,A1,B1,EPS1)
#if defined(CERNLIB_DOUBLE)
      APPRX2=DGAUSS(D103F2,A,B,EPS2)
#endif
        LTEST1=LTEST1 .AND. OKPK
      RETURN
C
 2000 FORMAT( // ' ***** D103CH ... TEST FAILURE.' )
 2001 FORMAT( 4X, 'NFN =', I2, 4X, 'ERR1 =', 1PE9.1, 4X, 'EPS1 =', E9.1)
 2002 FORMAT( 4X, 'NFN =', I2, 4X, 'ERR2 =', 1PD9.1, 4X, 'EPS2 =', D9.1)
      END
      FUNCTION D103F2(X)

#include "gen/imp64.inc"
      COMMON /D103C2/ A,B,P,Q,DNF

*    exp(-80) is about all a VAX F and D float can handle
#if defined(CERNLIB_VAX)
      PARAMETER (C = 80)
#endif
#if !defined(CERNLIB_VAX)
      PARAMETER (C = 100)
#endif

      IF(DNF .EQ. 1) D103F2=P*EXP(MAX(-(P*(X-Q))**2,-C))
      IF(DNF .EQ. 2) D103F2=P*SQRT((B-A)**2-(2*X-B-A)**2)/2
      IF(DNF .EQ. 3) D103F2=P*(SIN(X)**2)*COS(Q*X)
      IF(DNF .EQ. 4) D103F2=P*(SIN(X)**2)*COS(Q*X)+P
      IF(DNF .EQ. 5) D103F2=1/X**2
      RETURN
      END
      FUNCTION D103F1(X)

      COMMON /D103C1/ A1,B1,P1,Q1,RNF

#if defined(CERNLIB_QIEEE)||defined(CERNLIB_VAX)
      PARAMETER (C = 80)
#endif
#if (!defined(CERNLIB_QIEEE))&&(!defined(CERNLIB_VAX))
      PARAMETER (C = 100)
#endif

      IF(RNF .EQ. 1) D103F1=P1*EXP(MAX(-(P1*(X-Q1))**2,-C))
      IF(RNF .EQ. 2) D103F1=P1*SQRT((B1-A1)**2-(2*X-B1-A1)**2)/2
      IF(RNF .EQ. 3) D103F1=P1*(SIN(X)**2)*COS(Q1*X)
      IF(RNF .EQ. 4) D103F1=P1*(SIN(X)**2)*COS(Q1*X)+P1
      IF(RNF .EQ. 5) D103F1=1/X**2
      RETURN
      END
C
      SUBROUTINE REPSIL(X,Y)
      X=X/2.
      Y= 1. + X
      RETURN
      END
      SUBROUTINE DEPSIL(X,Y)
      DOUBLE PRECISION X,Y
      X=X/2.
      Y= 1. + X
      RETURN
      END

