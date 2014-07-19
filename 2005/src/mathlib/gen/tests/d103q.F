*
* $Id: d103q.F,v 1.2 1996/11/25 16:17:24 cernlib Exp $
*
* $Log: d103q.F,v $
* Revision 1.2  1996/11/25 16:17:24  cernlib
* Protect the loop finding eps such that 1+eps!=1 against optimisation.
* Introduce a SUBROUTINE to do some of the work.
*
* Revision 1.1.1.1  1996/04/01 15:01:22  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
#if (defined(CERNLIB_QUAD))&&(!defined(CERNLIB_IBMRS))
      SUBROUTINE D103Q
C
#include "gen/imp128.inc"
      LOGICAL OKFN,OKPK
      LOGICAL LTEST2
      EXTERNAL D103F4
      COMMON /D103C4/ A,B,P,Q,DNF
      COMMON /D103LT2/LTEST2
#include "iorc.inc"
      DIMENSION OKFN (10)
      DIMENSION ERR (10)

      PARAMETER (R0 = 0, R1 = 1, R2 = 2, HF = R1/2)

      DATA MRATIO /200/

      LTEST2=.TRUE.

      G2=1
   11 CALL QEPSIL(G2,G2S)
      IF(G2S .NE. 1) GO TO 11
      RELPRT=2*G2
C     WRITE(LOUT,'(1X,2D12.4/)') RELPRT

      EPS =MRATIO*RELPRT
#if defined(CERNLIB_DOUBLE)
      PI = 3.14159 26535 89793 23846 26433 83279 50Q0
      WRITE(LOUT,'(4X,''NFN'',22X,''EXACT'',29X,''QGAUSS'')')
#endif
#if !defined(CERNLIB_DOUBLE)
      PI = 3.14159 26535 89793 23846 26433 83279 50D0
      WRITE(LOUT,'(4X,''NFN'',22X,''EXACT'',29X,''DGAUSS'')')
#endif
C  1. SHARP PEAK

      DNF=1
      NFN=1
      A=HF
      B=R1/10
      P=100
      Q=R2/10
      EXACT=-SQRT(PI)
#if !defined(CERNLIB_DOUBLE)
      APPRX =DGAUSS(D103F4,A,B,EPS)
#endif
#if defined(CERNLIB_DOUBLE)
      APPRX =QGAUSS(D103F4,A,B,EPS)
#endif
      ERR (NFN)=ABS(APPRX -EXACT)
      OKFN (NFN)=ERR (NFN) .LT. EPS
      WRITE(LOUT,'(I5,2F40.32/)') NFN,EXACT,APPRX
C
#if !defined(CERNLIB_QIEEE)
C  2. NON-POLYNOMIAL END-POINTS.

      DNF=2
      RNF=2
      NFN=2
      A=HF
      B=-3*HF
      P=1
      EXACT=SIGN(R1/8,B-A)*P*PI*(B-A)**2
#endif
#if !defined(CERNLIB_DOUBLE)
      APPRX =DGAUSS(D103F4,A,B,EPS)
#endif
#if (defined(CERNLIB_DOUBLE))&&(!defined(CERNLIB_QIEEE))
      APPRX =QGAUSS(D103F4,A,B,EPS)
#endif
#if !defined(CERNLIB_QIEEE)
      ERR (NFN)=ABS(APPRX -EXACT)
      OKFN (NFN)=ERR (NFN) .LT. EPS
      WRITE(LOUT,'(I5,2F40.32/)') NFN,EXACT,APPRX
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
#endif
#if !defined(CERNLIB_DOUBLE)
      APPRX =DGAUSS(D103F4,A,B,EPS)
#endif
#if (defined(CERNLIB_DOUBLE))&&(!defined(CERNLIB_QIEEE))
      APPRX =QGAUSS(D103F4,A,B,EPS)
#endif
#if !defined(CERNLIB_QIEEE)
      ERR (NFN)=ABS(APPRX -EXACT)
      OKFN (NFN)=ERR (NFN) .LT. EPS
      WRITE(LOUT,'(I5,2F40.32/)') NFN,EXACT,APPRX
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
#endif
#if !defined(CERNLIB_DOUBLE)
      APPRX =DGAUSS(D103F4,A,B,EPS)
#endif
#if (defined(CERNLIB_DOUBLE))&&(!defined(CERNLIB_QIEEE))
      APPRX =QGAUSS(D103F4,A,B,EPS)
#endif
#if !defined(CERNLIB_QIEEE)
      ERR (NFN)=ABS((APPRX -EXACT)/EXACT)
      OKFN (NFN)=ERR (NFN) .LT. EPS
      WRITE(LOUT,'(I5,2E40.32/)') NFN,EXACT,APPRX
#endif
      OKPK=.TRUE.
      DO 1 I=1,NFN
    1 OKPK=OKPK .AND. OKFN (I)
      IF(.NOT.OKPK) THEN
       WRITE(LOUT,2000)
       DO 2 I=1,NFN
       IF(.NOT.OKFN (I)) WRITE(LOUT,2001) I,ERR (I),EPS
    2  CONTINUE
      ENDIF
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
      DNF=5
      NFN=5
      A=1
      B=0
#if !defined(CERNLIB_DOUBLE)
      APPRX =DGAUSS(D103F4,A,B,EPS)
#endif
#if defined(CERNLIB_DOUBLE)
      APPRX =QGAUSS(D103F4,A,B,EPS)
#endif
      WRITE(LOUT,'(1X)')
        LTEST2=LTEST2 .AND. OKPK
      RETURN
C
 2000 FORMAT( // ' ***** D103CH ... TEST FAILURE.' )
 2001 FORMAT( 4X, 'NFN =', I2, 4X, 'ERR  =', 1PE9.1, 4X, 'EPS  =', E9.1)
      END
      FUNCTION D103F4(X)

#include "gen/imp128.inc"
      COMMON /D103C4/ A,B,P,Q,DNF

#if defined(CERNLIB_VAX)
      PARAMETER (C = 80)
#endif
#if !defined(CERNLIB_VAX)
      PARAMETER (C = 100)
#endif


      IF(DNF .EQ. 1) D103F4=P*EXP(MAX(-(P*(X-Q))**2,-C))
      IF(DNF .EQ. 2) D103F4=P*SQRT((B-A)**2-(2*X-B-A)**2)/2
      IF(DNF .EQ. 3) D103F4=P*(SIN(X)**2)*COS(Q*X)
      IF(DNF .EQ. 4) D103F4=P*(SIN(X)**2)*COS(Q*X)+P
      IF(DNF .EQ. 5) D103F4=1/X**2
      RETURN
      END
#endif
      SUBROUTINE QEPSIL(X,Y)
#include "gen/imp128.inc"
      X=X/2.
      Y= 1. + X
      RETURN
      END

