*
* $Id: c303m.F,v 1.1.1.1 1996/04/01 15:01:14 mclareni Exp $
*
* $Log: c303m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:14  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C303M
C     Program to test the MATHLIB routines GAMMF and DGAMMF (C303)

#include "imp64r.inc"
      REAL GAMMF
C     Set maximum error allowed for test to be considered successful
      DIMENSION TOL(2)

#include "gen/def64.inc"
     + Z1,X,C
      LOGICAL LTEST
      PARAMETER (Z1 = 1)
      DIMENSION C(0:20)
#include "iorc.inc"
      DATA TOL/1D-6, 5D-14/
      DATA LTEST/.TRUE./
      CALL HEADER('C303',0)

      PI = 3.14159 26535 89793D0
      C(0)=1
      DO 2 N = 1,20
    2 C(N)=(2*N-1)*C(N-1)/2
#if defined(CERNLIB_DOUBLE)
      NF=2
#endif
#if !defined(CERNLIB_DOUBLE)
      NF=1
#endif
      DO 1000 JF=1,NF
         ERRMAX= 0.0D0
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/10X,''TEST FOR GAMMF'')')
      WRITE(LOUT,'(/9X,''X  '',7X,''EXACT'',20X,''GAMMF'',
     + 13X,''Rel. Error'')')
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)WRITE(LOUT,'(/10X,''TEST FOR GAMMF'')')
      IF(JF.EQ.2)WRITE(LOUT,'(/10X,''TEST FOR DGAMMF'')')
      WRITE(LOUT,'(/9X,''X  '',7X,''EXACT'',17X,''CALCULATED'',
     + 13X,''Rel. Error'')')
#endif
      DO 1 N = -3,20
      IF(N .EQ. 0) GO TO 1
      X=N+Z1/2
      T=C(ABS(N))*SQRT(PI)
      IF(X .LT. 0) T=PI/(SIN(PI*X)*T)
#if !defined(CERNLIB_DOUBLE)
      DR=GAMMF(X)
      IF(DR .NE. 0) ER=ABS((DR-T)/DR)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                 RT=T
                 RX=X
                 DR=GAMMF(RX)
      IF(DR .NE. 0) ER=ABS((DR-RT)/DR)
                 X=RX
                 T=RT
      ENDIF
      IF(JF.EQ.2)THEN
      DR=DGAMMF(X)
      IF(DR .NE. 0) ER=ABS((DR-T)/DR)
      ENDIF
#endif
         ERRMAX= MAX( ERRMAX,ER )
      WRITE(LOUT,'(1X,F10.1,2D25.15,D10.1)') X,T,DR,ER
    1 CONTINUE
#if !defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF+1)
#endif
#if defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF)
#endif
      WRITE(LOUT,'(/''LARGEST RELATIVE ERROR WAS'',1P,D10.1)')
     +ERRMAX
      LTEST=LTEST.AND.(ERRMAX.LE.ETOL)
         ERRMAX= 0D0
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:'')')
#if !defined(CERNLIB_DOUBLE)
      DR=GAMMF(-Z1)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                 RZ1=Z1
                 DR=GAMMF(-RZ1)
      ENDIF
      IF(JF.EQ.2)DR=DGAMMF(-Z1)
#endif
1000  CONTINUE
C     Check if the test was successful
      IRC=ITEST('C303',LTEST)
      CALL PAGEND('C303')
      END
