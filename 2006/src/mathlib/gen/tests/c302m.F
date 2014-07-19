*
* $Id: c302m.F,v 1.1.1.1 1996/04/01 15:01:13 mclareni Exp $
*
* $Log: c302m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:13  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C302M
C     Program to test the MATHLIB routines GAMMA, DGAMMA and
C     QGAMMA (C302)

      LOGICAL LTEST, LTEST1,LTEST2
      COMMON /C302LT1/LTEST1
#if defined(CERNLIB_QUAD)
      COMMON /C302LT2/LTEST2
#endif
#include "iorc.inc"
      CALL HEADER('C302',0)
      LTEST=.TRUE.
      LTEST1=.TRUE.
      LTEST2=.TRUE.
      CALL C302D
        LTEST=LTEST .AND. LTEST1
#if defined(CERNLIB_QUAD)
      CALL C302Q
        LTEST=LTEST .AND. LTEST2
#endif
      IRC=ITEST('C302',LTEST)
      CALL PAGEND('C302')
      RETURN
      END
      SUBROUTINE C302D

#include "gen/imp64.inc"

      REAL GAMMA
      CHARACTER*6 TFUNC(2)
#include "iorc.inc"
C
      PARAMETER (HALF = 5D-1, PI=3.14159 26535 89793 24D0)
      DIMENSION C(0:20)
      LOGICAL LTEST1
      DIMENSION TOL(2)
      COMMON /C302LT1/LTEST1
C
C     Set maximum error allowed for test to be considered successful
C
#if (!defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_IBMRT))
      DATA TOL/1D-6, 5D-14/
#endif
#if defined(CERNLIB_IBMVM)||defined(CERNLIB_IBMRT)
      DATA TOL/1D-4, 1D-14/
#endif

      DATA TFUNC/'GAMMA','DGAMMA'/
      LTEST1=.TRUE.

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
        RERRMAX= 0.0E0
         WRITE(LOUT,'(/10X,''Test of C302 '',A)') TFUNC(JF)
         WRITE(LOUT,'(/9X,''X  '',7X,''Exact'',25X,''Calculated'',
     + 14X,''Rel. Error'')')

      DO 1 N =  1,20
                   X=N+HALF
                   T=C(ABS(N))*SQRT(PI)
#if !defined(CERNLIB_DOUBLE)
                       DR=GAMMA(X)
         IF(DR .NE. 0) ER=ABS((DR-T)/DR)
         WRITE(LOUT,'(1X,F10.1,2E27.18,5X,E10.1)') X,T,DR,ER
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1) THEN
                        DR=GAMMA( SNGL(X) )
         IF(DR .NE. 0) ER=ABS(SNGL(DR-T)/SNGL(DR) )
         WRITE(LOUT,'(1X,F10.1,2E27.9,5X,E10.1)')
     +        SNGL(X),SNGL(T),SNGL(DR),SNGL(ER)

      ENDIF
      IF(JF.EQ.2) THEN
                       DR=DGAMMA(X)
         IF(DR .NE. 0) ER=ABS((DR-T)/DR)
         WRITE(LOUT,'(1X,F10.1,2E27.18,5X,E10.1)') X,T,DR,ER
      ENDIF
#endif
      ERRMAX= MAX( ERRMAX,ER )
    1 CONTINUE
#if !defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF+1)
#endif
#if defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF)
#endif
      WRITE(LOUT,'(/''Largest relative error was'',E10.1)') ERRMAX
      LTEST1=LTEST1.AND.(ERRMAX.LE.ETOL)

      WRITE(LOUT,'(/''Testing error messages:'')')
#if !defined(CERNLIB_DOUBLE)
                  DR=GAMMA(-HALF)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1) DR=GAMMA(-SNGL(HALF))
      IF(JF.EQ.2) DR=DGAMMA(-HALF)
#endif
1000  CONTINUE
      RETURN
      END

