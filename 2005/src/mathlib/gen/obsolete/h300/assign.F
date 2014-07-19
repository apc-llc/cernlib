*
* $Id: assign.F,v 1.1.1.1 1996/04/01 15:03:17 mclareni Exp $
*
* $Log: assign.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:17  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE ASSIGN(A,N,M,IDIM1,K,TOTAL,MODE)
C
      DIMENSION A(IDIM1,1),K(N)
      DIMENSION L(100)
      INTEGER CBL, CL, CL0, RL, RS, SW, P
      INTEGER C(100), CB(100), LAMBDA(100), MU(100), R(100)
      EQUIVALENCE (R(1),NEWK),(CB(1),NEWL)
C
C     ******************************************************************
C
C     A            IS A TWO-DIMENSIONAL REAL ARRAY CONTAINING A RECTAN-
C                  GULAR MATRIX. THIS MATRIX IS DESTROYED BY THE PROGRAM
C     N (INTEGER)  IS THE NUMBER OF ROWS IN THE MATRIX.
C     M (INTEGER)  IS THE NUMBER OF COLUMNS IN THE MATRIX.
C     IDIM1 (INT.) IS THE FIRST DIMENSION PARAMETER OF A AS DECLARED
C                  IN THE CALLING PROGRAM.
C     K (INTEGER)  IS A ONE-DIMENSIONAL ARRAY.
C     TOTAL        IS A REAL VARIABLE.
C     MODE (INT.)  MUST HAVE THE VALUE 1 OR 2
C
C     MODE = 1
C     --------
C
C                  K(1),K(2),...,K(N) ARE ASSIGNED INTEGER VALUES WHICH
C     MINIMIZE
C                  A(1,K(1))+A(2,K(2))+...+A(I,K(I))+...+A(N,K(N))   (1)
C
C     AND TOTAL IS SET EQUAL TO THE MINIMUM VALUE.
C
C                  IF M .GT. N , THE K(I) ARE DISTINCT AND ARE A SUBSET
C     OF THE INTEGERS 1,2,...,M.
C
C                  IF M = N , THE K(I) ARE A PERMUTATION OF THE INTEGERS
C     1,2,...,N.
C
C                  IF M .LT. N , THE SET OF K(I) CONSISTS OF SOME
C     PERMUTATION OF THE INTEGERS 1,2,...,M , INTERSPERCED WITH N-M
C     ZEROS. THE PERMUTATION AND THE POSITIONS OF ZEROS ARE CHOOSEN IN
C     SUCH A WAY AS TO MINIMIZE THE SUM (1) WITH THE CONVENTION THAT
C     A(I,0) IS TO BE TAKEN EQUAL TO ZERO.
C
C     MODE = 2
C     --------
C
C                  K(1),K(2),...,K(M) ARE ASSIGNED INTEGER VALUES WHICH
C     MINIMIZE
C                  A(K(1),1)+A(K(2),2)+...+A(K(J),J)+...+A(K(M),M)   (2)
C
C     AND TOTAL IS SET EQUAL TO THE MINIMUM VALUE. IF M .GT. N , M-N
C     OF THE K(J) ARE SET EQUAL TO ZERO , WITH THE CONVENTION THAT
C     A(0,J) IS TO BE TAKEN EQUAL TO ZERO.
C
C     METHOD
C     ------
C
C                  THE PROGRAM IS BASED ON THE ALGOL PROCEDURE OF
C     SILVER (1960) WHICH USES THE ASSIGNMENT ALGORITHM OF
C     MUNKRES (1957). SILVER"S PROCEDURE HAS BEEN EXTENDED TO HANDLE THE
C     CASE M .NE. N.
C
C     TIMING
C     ------
C
C                  APPROXIMATE TIMES ARE GIVEN BY THE FOLLOWING FORMULAS
C
C     CDC 6600     0.02*MAX*MIN**2 MILLISECONDS
C
C     CDC 6500     0.05*MAX*MIN**2 MILLISECONDS
C
C     WHERE  MAX = MAX(N,M)  AND  MIN = MIN(N,M) .
C
C     ******************************************************************
C
C-----INITIALIZE
C
      IMAX=MAX(N,M)
      IMIN=MIN(N,M)
      TOTAL=0.0
      IF(N.GT.M) GO TO 100
C
      DO 1 I=1,N
      RMIN=A(I,1)
C
      DO 2 J=1,M
      RMIN=MIN(RMIN,A(I,J))
    2 CONTINUE
C
      TOTAL=TOTAL+RMIN
C
      DO 3 J=1,M
      A(I,J)=A(I,J)-RMIN
    3 CONTINUE
C
    1 CONTINUE
C
      IF(N.LT.M) GO TO 110
  100 DO 4 J=1,M
      RMIN=A(1,J)
C
      DO 5 I=1,N
      RMIN=MIN(RMIN,A(I,J))
    5 CONTINUE
C
      TOTAL=TOTAL+RMIN
C
      DO 7 I=1,N
      A(I,J)=A(I,J)-RMIN
    7 CONTINUE
C
    4 CONTINUE
C
  110 DO 8 I=1,IMAX
      K(I)=0
      L(I)=0
    8 CONTINUE
C
      DO 12 I=1,N
C
      DO 13 J=1,M
      IF(A(I,J)+L(J).NE.0.) GO TO 13
      K(I)=J
      L(J)=I
      GO TO 12
   13 CONTINUE
C
   12 CONTINUE
C
C-----(START) START LABELLING
   10 IFLAG=N
      RL=0
      CL=0
      RS=1
C
      DO 11 I=1,N
      MU(I)=0
      IF(K(I).NE.0) GO TO 11
      RL=RL+1
      R(RL)=I
      MU(I)=-1
      IFLAG=IFLAG-1
   11 CONTINUE
C
      IF(IFLAG.EQ.IMIN) GO TO 70
C
      DO 14 J=1,M
      LAMBDA(J)=0
   14 CONTINUE
C
C-----(LABEL) LABEL AND SCAN
   30 I=R(RS)
      RS=RS+1
C
      DO 31 J=1,M
      IF(A(I,J)+LAMBDA(J).NE.0.) GO TO 31
      LAMBDA(J)=I
      CL=CL+1
      C(CL)=J
      NEWSUB=L(J)
      IF(NEWSUB.EQ.0) GO TO 60
      RL=RL+1
      R(RL)=NEWSUB
      MU(NEWSUB)=I
   31 CONTINUE
C
      IF(RS.LE.RL) GO TO 30
C
C-----RENORMALIZE
C*UL   40 SW=1
      SW=1
      CL0=CL
      CBL=0
C
      DO 41 J=1,M
      IF(LAMBDA(J).NE.0) GO TO 41
      CBL=CBL+1
      CB(CBL)=J
   41 CONTINUE
C
      RMIN=A(NEWK,NEWL)
C
      DO 42 I=1,RL
C
      DO 43 J=1,CBL
      NEW1=R(I)
      NEW2=CB(J)
      RMIN=MIN(RMIN,A(NEW1,NEW2))
   43 CONTINUE
C
   42 CONTINUE
C
      TOTAL=TOTAL+RMIN*(RL+CBL-IMAX)
C
      DO 44 I=1,N
      IF(MU(I).NE.0) GO TO 46
C
      IF(CL0.LT.1) GO TO 44
      DO 49 P=1,CL0
      NEWSUB=C(P)
      A(I,NEWSUB)=A(I,NEWSUB)+RMIN
   49 CONTINUE
C
      GO TO 44
C
   46 DO 45 P=1,CBL
      NEWSUB=CB(P)
      A(I,NEWSUB)=A(I,NEWSUB)-RMIN
      GO TO (47,45),SW
C
C-----(NEXT)
   47 IF(A(I,NEWSUB)+LAMBDA(NEWSUB).NE.0.) GO TO 45
      LAMBDA(NEWSUB)=I
      IF(L(NEWSUB).NE.0) GO TO 48
      J=NEWSUB
      SW=2
      GO TO 45
   48 CL=CL+1
      C(CL)=NEWSUB
      RL=RL+1
      R(RL)=L(NEWSUB)
   45 CONTINUE
C
   44 CONTINUE
      GO TO (50,60),SW
C
C-----(NEXT1)
   50 IF(CL0.EQ.CL) GO TO 30
      ITEMP=CL0+1
C
      DO 51 I=ITEMP,CL
      NEWSUB=C(I)
      NEWSUB=L(NEWSUB)
      MU(NEWSUB)=C(I)
   51 CONTINUE
C
      GO TO 30
C
C-----(MARK) MARK NEW COLUMN AND PERMUTE
   60 L(J)=LAMBDA(J)
      I=LAMBDA(J)
      IF(K(I).NE.0) GO TO 61
      K(I)=J
      GO TO 10
C
   61 JSV=J
      J=K(I)
      K(I)=JSV
      GO TO 60
C
   70 IF(MODE.EQ.1) RETURN
C
      DO 71 I=1,IMAX
      K(I)=L(I)
   71 CONTINUE
C
      RETURN
C
      END
