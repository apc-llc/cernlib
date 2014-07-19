*
* $Id: d106ch.F,v 1.1.1.1 1996/02/15 17:48:40 mclareni Exp $
*
* $Log: d106ch.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:40  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
#if defined(CERNLIB_NEVER)
      SUBROUTINE D106CH(NREP,X,W,OKPK)
      LOGICAL OKPK
      EXTERNAL D106FN
      DIMENSION X(2),W(2),NTAB(24),ATAB(5),BTAB(5)
#include "kernnumt/sysdat.inc"
      COMMON /D106CM/IFN,A,B
      DATA NTAB/2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,20,24,32,40,48,64,
     +          80,96,-999/
      DATA ATAB/-1.0,-1.0,-1.0,0.1,-0.2/
      DATA BTAB/+1.0,+1.0,+1.0,1.8,-1.7/
#if defined(CERNLIB_NUMCR1)||defined(CERNLIB_NUMCV1)
      DATA MRATIO/100/
#endif
#if (!defined(CERNLIB_NUMCR1))&&(!defined(CERNLIB_NUMCV1))
      DATA MRATIO/50/
#endif
C
C  TEST-ROUTINE FOR D106 (GQUAD,GSET).
C  CALLS ... FUNCTION GQUAD, SUBROUTINE GSET.
C        ... FUNCTION D106FN.
C
C  START.
      OKPK=.FALSE.
      IF(NREP.LE.0) RETURN
      EPS=RELPR*FLOAT(MRATIO)
      ITEST=0
      IFAIL=0
      RELMAX=0.
      DO 3 JN=1,50
         N=NTAB(JN)
         IF(N.LT.0) GO TO 4
         IMAX=3
         IF(N.EQ.16) IMAX=5
         DO 2 IFN=1,IMAX
            A=ATAB(IFN)
            B=BTAB(IFN)
            CALL GSET(A,B,N,X,W)
C           (BEGIN EXPLICIT GAUSS SUM).
            QSUM=0.
            M=N/2
            DO 1 K=1,M
               WK=W(K)
               L=N-K+1
               IF(W(L).NE.WK) REL=1.
               QSUM = QSUM + WK*( D106FN(X(K)) + D106FN(X(L)) )
    1       CONTINUE
            IF(N.NE.2*M) QSUM = QSUM + D106FN(X(M+1))
C           (END EXPLICIT GAUSS SUM).
            QGAUSS=GQUAD(D106FN,A,B,N)
            IF(QSUM.NE.QGAUSS) REL=1.
            IF(IFN.EQ.1) QREF=2D0
            IF(IFN.EQ.2) QREF=2D0/3D0
            IF(IFN.EQ.3) QREF=4D0/3D0
            IF(IFN.GE.4) QREF=DCOS(DBLE(A))-DCOS(DBLE(B))
            REL=(QGAUSS-QREF)/ABS(QREF)
            IF(ABS(REL).GE.EPS) IFAIL=IFAIL+1
            RELMAX=AMAX1(RELMAX,ABS(REL))
            ITEST=ITEST+1
    2    CONTINUE
    3 CONTINUE
    4 OKPK=IFAIL.EQ.0
      RATIO=RELMAX/RELPR
      IF(.NOT.OKPK) WRITE(*,2000) IFAIL,ITEST,RATIO
      IF(      ERPRNT .AND.       ERSTOP) WRITE(*,1001)
      IF(      ERPRNT .AND. .NOT. ERSTOP) WRITE(*,1002)
      IF(.NOT. ERPRNT .AND.       ERSTOP) WRITE(*,1003)
      N  =  1
      CALL GSET(A,B,N,X,W)
      N  =  17
      QGAUSS  =  GQUAD(D106FN,A,B,N)
      RETURN
 1001 FORMAT(/'  TWO ERROR AND ABEND MESSAGES SHOULD NOW FOLLOW ...')
 1002 FORMAT(/'  TWO ERROR MESSAGES SHOULD NOW FOLLOW ...')
 1003 FORMAT(/'  TWO ABEND MESSAGES SHOULD NOW FOLLOW ...')
 2000 FORMAT( // 18H ***** D106CH ... , I4, 15H  FAILURES IN  ,
     *        I4, 25H  TESTS.  RELMAX/RELPR = , 1P, E8.1 )
      END
#endif
