*
* $Id: e105ch.F,v 1.1.1.1 1996/02/15 17:48:41 mclareni Exp $
*
* $Log: e105ch.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:41  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
      SUBROUTINE E105CH(NREP,NTAB,A,B,F,OKPK)
      LOGICAL OKPK
      DIMENSION A(NTAB),B(NTAB),F(NTAB)
#include "kernnumt/sysdat.inc"
      DATA MRATIO/50/, MMAX/12/
C
C  TEST-ROUTINE FOR E105 (DIVDIF).
C  CALLS ... SUBROUTINE DIVDIF.
C        ... CERN ROUTINE POLINT (E100).
C        ... TEST-ROUTINE E105TR.
C
C  START.  SET INCREASING ARGUMENTS IN A, DECREASING ARGUMENTS IN B.
      FBIG=ERANGE/100.
      OKPK=.FALSE.
      IF(NREP.LE.0) RETURN
      EPS=RELPR*FLOAT(MRATIO)
      ITEST=0
      IFAIL=0
      RELMXA=0.
      RELMXB=0.
      JB=NTAB
      DO 1 JA=1,NTAB
         T=ALOG(FLOAT(JA+5))
         A(JA)=T
         B(JB)=T
         JB=JB-1
    1 CONTINUE
C
C  TEST DIVDIF BY COMPARING RESULTS WITH TRANSPARENT VERSION E105TR.
      DO 7 M=1,MMAX
         NMAX=M+5
         DO 6 N=2,NMAX
            NPLUS=N+1
C
C           (SET F ARRAY.)
            RA=RANF()
            RB=RANF()
            DO 2 I=1,NPLUS
               F(I)=SQRT(RA*A(I)+RB*B(I))
    2       CONTINUE
C
C           (CHOOSE ARGUMENTS X FROM ALL INTERVALS IN TURN.)
            DO 5 J=1,NPLUS
               DO 4 L=1,2
                  IF((J.EQ.1).AND.(L.EQ.1)) GO TO 4
                  XA=A(J)
                  XB=B(J)
                  IF(L.EQ.1) GO TO 3
                     T=RANF()
                     XA=T*A(J)+(1.-T)*A(J+1)
                     XB=T*B(J)+(1.-T)*B(J+1)
    3             F(1)=-FBIG
                  F(N+2)=-FBIG
                  YA=DIVDIF(F(2),A(2),N,XA,M)
                  YB=DIVDIF(F(2),B(2),N,XB,M)
                  F(1)=+FBIG
                  F(N+2)=+FBIG
                  YAREF=E105TR(F(2),A(2),N,XA,M)
                  YBREF=E105TR(F(2),B(2),N,XB,M)
                  RELA=ABS((YA-YAREF)/YAREF)
                  RELB=ABS((YB-YBREF)/YBREF)
                  IF(RELA.GT.EPS) IFAIL=IFAIL+1
                  IF(RELB.GT.EPS) IFAIL=IFAIL+1
                  ITEST=ITEST+2
                  RELMXA=AMAX1(RELA,RELMXA)
                  RELMXB=AMAX1(RELB,RELMXB)
    4          CONTINUE
    5       CONTINUE
    6    CONTINUE
    7 CONTINUE
      OKPK=IFAIL.EQ.0
      IF(OKPK) GOTO 9
         ARATIO=RELMXA/RELPR
         BRATIO=RELMXB/RELPR
         WRITE(*,2000) IFAIL,ITEST,ARATIO,BRATIO
    9 CONTINUE
      IF(      ERPRNT .AND.       ERSTOP) WRITE(*,101)
      IF(      ERPRNT .AND. .NOT. ERSTOP) WRITE(*,102)
      IF(.NOT. ERPRNT .AND.       ERSTOP) WRITE(*,103)
      N  =  1000
      M  =  0
      Y  =  DIVDIF(F,A,N,X,M)
      N  =  1
      M  =  1
      Y  =  DIVDIF(F,A,N,X,M)
      RETURN
  101 FORMAT(/' TWO ERROR AND ABEND MESSAGES SHOULD NOW FOLLOW ...')
  102 FORMAT(/' TWO ERROR MESSAGES SHOULD NOW FOLLOW ...')
  103 FORMAT(/' TWO ABEND MESSAGES SHOULD NOW FOLLOW ...')
 2000 FORMAT( // 18H ***** E105CH ... ,  I5, 13H FAILURES IN , 1P,
     *        I5, 8H TESTS.
     *        / 5X, 14HRELMXA/RELPR =, E9.1, 5X, 14HRELMXB/RELPR =,
     *        E9.1, 1H. )
      END
      FUNCTION E105TR(F,A,N,X,MM)
      DIMENSION A(N),F(N)
      LOGICAL EXTRA
      DATA MMAX/10/
C
C  TRANSPARENT VERSION OF DIVDIF (E105).
C
C  START.  FIND SUBSCRIPT IX OF X IN ARRAY A.
      IX=0
      EPREV=SIGN(1.0,A(2)-A(1))
    1    E=X-A(IX+1)
         IF((EPREV.EQ.0).OR.(E*EPREV.LT.0)) GO TO 2
         EPREV=E
         IX=IX+1
         IF(IX.LT.N) GO TO 1
C
C  CHOOSE INTERPOLATION POINTS.
    2 M=MIN0(MM,MMAX,N-1)
      NPTS=M+1
      EXTRA=MOD(M,2).EQ.0
      IF(EXTRA) NPTS=NPTS+1
      IFIRST=IX-(NPTS/2)+1
      ILAST=IX+(NPTS/2)
      IF(IFIRST.GE.1) GO TO 3
         IFIRST=1
         EXTRA=.FALSE.
         GO TO 4
    3 IF(ILAST.LE.N) GO TO 4
         NPTS=M+1
         IFIRST=N-NPTS+1
         EXTRA=.FALSE.
C
C  PERFORM THE INTERPOLATION, AVERAGING IF EXTRA IS TRUE.
    4 CALL POLINT(F(IFIRST),A(IFIRST),M+1,X,Y1)
      IF(.NOT.EXTRA) GO TO 5
         CALL POLINT(F(IFIRST+1),A(IFIRST+1),M+1,X,Y2)
         Y1=0.5*(Y1+Y2)
    5 E105TR=Y1
      RETURN
      END
