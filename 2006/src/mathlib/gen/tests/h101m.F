*
* $Id: h101m.F,v 1.1.1.1 1996/04/01 15:01:30 mclareni Exp $
*
* $Log: h101m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:30  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE H101M
C     H101   RSMPLX,  DSMPLX

#if defined(CERNLIB_DOUBLE)
#include "gen/imp64.inc"
#endif

#include "iorc.inc"

      CHARACTER*40 TITLE,TEXT
      LOGICAL LER

      PARAMETER (IDA = 4, ND = 7, NEX = 10)
      PARAMETER (IDW = IDA+2*ND)

      DIMENSION AA(IDA,ND,NEX),BB(IDA,NEX),CC(ND,NEX)
      DIMENSION ZZ(NEX),MM1(NEX),MM(NEX),NN1(NEX),NN(NEX)
      DIMENSION A(IDA,ND),B(IDA),C(ND)
      DIMENSION LW(IDW,5),TITLE(NEX)
      DIMENSION W(IDA+ND),X(IDA+ND)
      DIMENSION TEXT(4)
      DIMENSION IX(IDA,NEX),IY(ND,NEX),KEY(NEX),IRS(NEX)

      DATA MM1(1),MM(1),NN1(1),NN(1) /4, 4, 3, 3/
      DATA ((AA(I,K,1),I=1,4),K=1,3)
     1  / -0.04D0, -60, 0.25D0, 9,   -0.02D0, -90, 0.5D0,  3,
     2          1,   0,     0,  0/
      DATA (BB(I,1),I=1,4),ZZ(1) /-0.02D0, 150, -0.75D0, 6,   0/
      DATA (CC(K,1),K=1,3) /0, 0, 1/
      DATA (IX(I,1),I=1,4),KEY(1),IRS(1) /100, 0, 4, 0,  11, 5/
      DATA (IY(K,1),K=1,3) /3, 0, 0/

      DATA MM1(2),MM(2),NN1(2),NN(2) /0, 2, 6, 6/
      DATA ((AA(I,K,2),I=1,2),K=1,6)
     1  / -2, 5,   -2, 15,   2, 5,   2, 1,   6, -1,   1, -1/
      DATA (BB(I,2),I=1,2),ZZ(2) /-1, -1,   0/
      DATA (CC(K,2),K=1,6) /20, 0, -40, -16, 0, 10/
      DATA (IX(I,2),I=1,2),KEY(2),IRS(2) /-500, -600,  21, -1100/
      DATA (IY(K,2),K=1,6) /4000, 8000, 0, 0, 2400, 900/

      DATA MM1(3),MM(3),NN1(3),NN(3) /0, 2, 6, 7/
      DATA ((AA(I,K,3),I=1,2),K=1,7)
     1  / -2, 5,   -2, 15,   2, 5,   2, 1,   6, -1,   1, -1,   -10, 3/
      DATA (BB(I,3),I=1,2),ZZ(3) /-1, -1,   0/
      DATA (CC(K,3),K=1,7) /20, 0, -40, -16, 0, 10, 0/
      DATA (IX(I,3),I=1,2),KEY(3),IRS(3) /-300, -1000,  31, -1300/
      DATA (IY(K,3),K=1,7) /6400, 14400, 1600, 0, 800, 300, 0/

      DATA MM1(4),MM(4),NN1(4),NN(4) /2, 2, 3, 3/
      DATA ((AA(I,K,4),I=1,2),K=1,3)
     1  / 10, 20,   1, 4,   1,1/
      DATA (BB(I,4),I=1,2),ZZ(4) /-1, -2,   0/
      DATA (CC(K,4),K=1,3) /1100, 160, 100/
      DATA (IX(I,4),I=1,2),KEY(4),IRS(4) /6000, 2500,  42, 11000/
      DATA (IY(K,4),K=1,3) /0, 0, 1500/

      DATA MM1(5),MM(5),NN1(5),NN(5) /0, 2, 7, 7/
      DATA ((AA(I,K,5),I=1,2),K=1,7)
     1  / -2, 5,   -2, 15,   2, 5,   2, 1,   6, -1,   1, -1,   -1, 0/
      DATA (BB(I,5),I=1,2),ZZ(5) /-1, -1,   0/
      DATA (CC(K,5),K=1,7) /20, 0, -40, -16, 0, 10, -1/
      DATA (IX(I,5),I=1,2),KEY(5),IRS(5) /0, 0,  54, 0/
      DATA (IY(K,5),K=1,7) /0, 0, 0, 0, 0, 0, 0/

      DATA MM1(6),MM(6),NN1(6),NN(6) /0, 2, 6, 6/
      DATA ((AA(I,K,6),I=1,2),K=1,6)
     1  / -2, 5,   -2, 15,   2, 5,   2, 1,   6, -1,   1, -1/
      DATA (BB(I,6),I=1,2),ZZ(6) /1, 1,   0/
      DATA (CC(K,6),K=1,6) /20, 0, -40, -16, 0, 10/
      DATA (IX(I,6),I=1,2),KEY(6),IRS(6) /0, 0,  63, 0/
      DATA (IY(K,6),K=1,6) /0, 0, 0, 0, 0, 0/

      DATA MM1(7),MM(7),NN1(7),NN(7) /0, 2, 6, 6/
      DATA ((AA(I,K,7),I=1,2),K=1,6)
     1  / -2, 5,   -2, 15,   2, 5,   2, 1,   6, -1,   1, -1/
      DATA (BB(I,7),I=1,2),ZZ(7) /0, -1,   0/
      DATA (CC(K,7),K=1,6) /20, 0, -40, -16, 0, 10/
      DATA (IX(I,7),I=1,2),KEY(7),IRS(7) /-1500, -200,  71, -200/
      DATA (IY(K,7),K=1,6) /0, 0, 0, 1600, 8800, 2300/

      DATA MM1(8),MM(8),NN1(8),NN(8) /2, 2, 3, 3/
      DATA ((AA(I,K,8),I=1,2),K=1,3)
     1  / -1, -4,   -2, -3,   -2, -1/
      DATA (BB(I,8),I=1,2),ZZ(8) /1, 1,   0/
      DATA (CC(K,8),K=1,3) /-8, -12, -6/
      DATA (IX(I,8),I=1,2),KEY(8),IRS(8) /150, 300,  81, -450/
      DATA (IY(K,8),K=1,3) /550, 0, 0/

      DATA MM1(9),MM(9),NN1(9),NN(9) /2, 4, 0, 4/
      DATA ((AA(I,K,9),I=1,4),K=1,4)
     1  / -4, -9,  3, -7,   -5,  2, -7, -1,
     2   -10, -5,  2, -1,   -2,  3, -2,  3/
      DATA (BB(I,9),I=1,4),ZZ(9) /-4, -10, 4, 5,   -14/
      DATA (CC(K,9),K=1,4) /2, 12, -35, -11/
      DATA (IX(I,9),I=1,4),KEY(9),IRS(9) /200,400,-100,-700, 91, 7300/
      DATA (IY(K,9),K=1,4) /0, 0, 0, 0/

      DATA MM1(10),MM(10),NN1(10),NN(10) /2, 2, 3, 3/
      DATA ((AA(I,K,10),I=1,2),K=1,3)
     1  / 1, -1,   0, 1,   1, 1/
      DATA (BB(I,10),I=1,2),ZZ(10) /1, 0,   0/
      DATA (CC(K,10),K=1,3) /-2, 1, -1/
      DATA (IX(I,10),I=1,2),KEY(10),IRS(10) /0, 0,  104, 0/
      DATA (IY(K,10),K=1,3) /0, 0, 0/

      DATA TITLE( 1) /'DEGENERACIES'/
      DATA TITLE( 2) /'NO RESTRICTIONS FOR THE VARIABLES X(I)'/
      DATA TITLE( 3) /'EQUATION CONSTRAINT'/
      DATA TITLE( 4) /'MORE THAN 1 SOLUTION'/
      DATA TITLE( 5) /'NO FEASIBLE INITIAL SOLUTION EXISTS'/
      DATA TITLE( 6) /'MAXIMUM = INFINITE'/
      DATA TITLE( 7) /'MAXIMUM IN DEGENERATED CORNER'/
      DATA TITLE( 8) /'ALL THE C(K) ARE NEGATIVE'/
      DATA TITLE( 9) /'ONLY EQUALITIES AS CONSTRAINTS'/
      DATA TITLE(10) /'NO FEASIBLE INITIAL SOLUTION'/

      DATA TEXT(1) /'EXACTLY ONE FINITE SOLUTION'/
      DATA TEXT(2) /'MORE THAN ONE SOLUTION EXISTS'/
      DATA TEXT(3) /'NO FINITE SOLUTION (MAX = INFINITE)'/
      DATA TEXT(4) /'NO FEASIBLE INITIAL SOLUTION EXISTS'/

      CALL HEADER('H101',0)

      LER=.FALSE.
      DO 11 IPRT = 0,1
      DO 10 NUM = 1,NEX
      DO 1 J = 1,4
      M1=MM1(NUM)
      M=MM(NUM)
      N1=NN1(NUM)
    1 N=NN(NUM)
      DO 2 K = 1,N
      DO 2 I = 1,M
    2 A(I,K)=AA(I,K,NUM)
      DO 3 K = 1,N
    3 C(K)=CC(K,NUM)
      DO 4 I = 1,M
    4 B(I)=BB(I,NUM)
      Z0=ZZ(NUM)
#if defined(CERNLIB_DOUBLE)
      CALL DSMPLX(A,B,C,Z0,IDA,M,M1,N,N1,LW,IDW,W,X,Z,ITYPE)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL RSMPLX(A,B,C,Z0,IDA,M,M1,N,N1,LW,IDW,W,X,Z,ITYPE)
#endif
      LER=LER .OR. .NOT.(KEY(NUM) .EQ. 10*NUM+ITYPE)
      IF(ITYPE .EQ. 1 .OR. ITYPE .EQ. 2) THEN
       LER=LER .OR. .NOT.(IRS(NUM) .EQ. NINT(100*Z))
       DO 12 I = 1,M
   12  LER=LER .OR. .NOT.(IX(I,NUM) .EQ. NINT(100*X(I)))
       DO 13 K = 1,N
   13  LER=LER .OR. .NOT.(IY(K,NUM) .EQ. NINT(100*X(K+M)))
      ENDIF

      IF(IPRT .EQ. 0) THEN
       WRITE(LOUT,'(//7X,''*** EXAMPLE'',I3,5X,A40/)') NUM,TITLE(NUM)
       WRITE(LOUT,'(7X,''ITYPE '',I2,5X,A40//)') ITYPE,TEXT(ITYPE)
       IF(ITYPE .EQ. 1 .OR. ITYPE .EQ. 2) THEN
        WRITE(LOUT,120) (I,X(I),I+M,X(I+M),I=1,MIN(M,N))
        IF(M .LT. N) WRITE(LOUT,121) (K+M,X(K+M),K=M+1,N)
        IF(M .GT. N) WRITE(LOUT,122) (I,X(I),I=N+1,M)
        WRITE(LOUT,123) Z
       ENDIF
      ENDIF
   10 CONTINUE
   11 CONTINUE
      WRITE(LOUT,'(1X)')
      IF(.NOT.LER) WRITE(LOUT,'(7X,''H101  TEST SUCCESSFUL'')')
      IF(LER) WRITE(LOUT,'(7X,''H101  TEST FAILED'')')

      IRC= ITEST('H101', .NOT.LER )

      WRITE(LOUT,'(1X)')
#if defined(CERNLIB_DOUBLE)
      CALL DSMPLX(A,B,C,Z0,IDA,0,M1,N,N1,LW,IDW,W,X,Z,ITYPE)
      CALL DSMPLX(A,B,C,Z0,IDA,M,M1,1,2,LW,IDW,W,X,Z,ITYPE)
#endif
#if !defined(CERNLIB_DOUBLE)
      CALL RSMPLX(A,B,C,Z0,IDA,0,M1,N,N1,LW,IDW,W,X,Z,ITYPE)
      CALL RSMPLX(A,B,C,Z0,IDA,M,M1,1,2,LW,IDW,W,X,Z,ITYPE)
#endif

      CALL PAGEND('H101')

  120 FORMAT(7X,'X',I2,' = ',F12.4,29X,'X',I2,' = ',F12.4)
  121 FORMAT(7X,47X,'X',I2,' = ',F12.4)
  122 FORMAT(7X,'X',I2,' = ',F12.4)
  123 FORMAT(//7X,'MAXIMUM VALUE OF THE OBJECTIVE FUNCTION = ',F12.4)
      END
