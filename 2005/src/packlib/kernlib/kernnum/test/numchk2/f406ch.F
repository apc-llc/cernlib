*
* $Id: f406ch.F,v 1.1.1.1 1996/02/15 17:48:42 mclareni Exp $
*
* $Log: f406ch.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:42  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
      SUBROUTINE F406CH(NREP,NMAX,KMAX,A,ABAND,B,BREF,OKPK)
      LOGICAL OKPK
      DIMENSION FRATIO(2)
      DIMENSION A(2),ABAND(2),B(2),BREF(2)
      INTEGER NERR(5),MERR(5),IERR(5),KERR(5)
#include "kernnumt/sysdat.inc"
      CHARACTER*1 CH(2)
      DATA CH/'R','D'/
      DATA NERR/1,1,1,0,1/
      DATA MERR/0,0,-1,0,1/
      DATA IERR/0,1,1,1,1/
      DATA KERR/1,0,1,1,1/
      DATA FRATIO/10.,20./
C
C  TEST-ROUTINE FOR F406 (RBEQN, DBEQN).
C
C  THE NUMBER OF ELEMENTS IN THE ARRAYS SHOULD NOT BE LESS THAN
C     A         ... 2*NMAX**2
C     ABAND     ... 2*NMAX*(NMAX+1)
C     B,BREF    ... 2*N*KMAX
C
C  CALLS ... SUBROUTINES RBEQN AND DBEQN (BOTH IN F406).
C        ... CERN PACKAGES F002 AND F003.
C        ... TEST-ROUTINES F406RR, F406RD, F406SR, F406SD,
C            F406ZR, F406ZD.
C
C  START.
      OKPK=.FALSE.
      IF(NREP.LE.0) RETURN
      IDIM=NMAX
      OKPK=.TRUE.
      DO 5 ITYPE=1,2
         ITEST=0
         IFAIL=0
         RATMAX=0.
         RSIGN=1.
         DO 4 JREP=1,NREP
            DO 3 N=1,NMAX
               NZERO=0
               NMINUS=N-1
               DO 2 M=NZERO,NMINUS
C
C                 (RESIDUAL TEST).
                  DO 1 K=1,KMAX
                     IF(ITYPE.EQ.1)
     $                 CALL F406RR(N,M,K,KMAX,IDIM,A,ABAND,B,BREF,RESID)
                     IF(ITYPE.EQ.2)
     $                 CALL F406RD(N,M,K,KMAX,IDIM,A,ABAND,B,BREF,RESID)
                     ITEST=ITEST+1
                     IF(RESID.LT.0.) RSIGN=-1.
                     RATIO=ABS(RESID)/RELPRT(ITYPE)
                     IF(RATIO.GT.FRATIO(ITYPE)) IFAIL=IFAIL+1
                     RATMAX=AMAX1(RATMAX,RATIO)
    1             CONTINUE
C
C                 (SINGULARITY TEST).
                  IF(ITYPE.EQ.1) CALL F406ZR(N,M,IDIM,ABAND,B,ISING)
                  IF(ITYPE.EQ.2) CALL F406ZD(N,M,IDIM,ABAND,B,ISING)
                  ITEST=ITEST+1
                  IF(ISING.NE.-1) IFAIL=IFAIL+1
C
    2          CONTINUE
    3       CONTINUE
    4    CONTINUE
         IF(IFAIL.EQ.0) GO TO 5
            OKPK=.FALSE.
            RATMAX=RSIGN*RATMAX
            WRITE(*,2000) CH(ITYPE),IFAIL,ITEST,RATMAX
    5 CONTINUE
#if !defined(CERNLIB_NUMDE)
      IF(      ERPRNT .AND.       ERSTOP) WRITE(*,1001)
      IF(      ERPRNT .AND. .NOT. ERSTOP) WRITE(*,1002)
      IF(.NOT. ERPRNT .AND.       ERSTOP) WRITE(*,1003)
      DO 9 JERR=1,5
         N=NERR(JERR)
         M=MERR(JERR)
         I=IERR(JERR)
         K=KERR(JERR)
         CALL RBEQN(N,M,ABAND,I,IFAIL,K,B)
         CALL DBEQN(N,M,ABAND,I,IFAIL,K,B)
    9    CONTINUE
#endif
      RETURN
#if !defined(CERNLIB_NUMDE)
 1001 FORMAT(/'  TEN ERROR AND ABEND MESSAGES SHOULD NOW FOLLOW ...')
 1002 FORMAT(/'  TEN ERROR MESSAGES SHOULD NOW FOLLOW ...')
 1003 FORMAT(/'  TEN ABEND MESSAGES SHOULD NOW FOLLOW ...')
#endif
 2000 FORMAT( // 16H ***** F406CH.  , A1, 14HBEQN TEST ... , I4,
     $        14H  FAILURES IN , I4, 25H  TESTS.   RESMAX/RELPR =,
     $        1P, E9.1, 1H. )
      END
