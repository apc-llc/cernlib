*
* $Id: dsbeqn.F,v 1.1.1.1 1996/03/06 15:36:26 mclareni Exp $
*
* $Log: dsbeqn.F,v $
* Revision 1.1.1.1  1996/03/06 15:36:26  mclareni
* Add geane321 examples
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.50  by  S.Giani
*-- Author :
          SUBROUTINE          DSBEQN(N,M,A,IDIM,IFAIL,K,B)
C
C     ******************************************************************
C
C     SOLVES A BANDED SYSTEM OF LINEAR EQUATIONS USING CHOLESKY
C     DECOMPOSITION
C
C     N       ORDER OF THE BAND MATRIX.
C
C     M       BAND PARAMETER. NON-ZERO COEFFICIENTS ARE CONFINED TO
C             IABS(I-J).LE.M.
C
C     A       (DOUBLE PRECISION) TWO-DIMENSIONAL ARRAY CONTAINING THE
C             SUCCESSIVE ROWS OF THE BAND MATRIX
C
C     IDIM    FIRST DIMENSION PARAMETER OF ARRAYS ABAND AND B.
C
C     IFAIL   OUTPUT PARAMETER.   IFAIL= 0 ... NORMAL EXIT.
C                                 IFAIL=-1 ... SINGULAR MATRIX.
C
C     K       NUMBER OF COLUMNS OF THE MATRIX IN ARRAY B.
C
C     B       (DOUBLE PRECISION) TWO-DIMENSIONAL ARRAY CONTAINING A
C             MATRIX OF RIGHT-HAND SIDES.
C
C     THIS SUBROUTINE REPLACES B BY THE SOLUTION MATRIX X OF A*X=B,
C     WHERE A IS THE BAND-MATRIX
C
C
C     Author. V.Innocente  -  November 1988
C
C     ORIGIN    KERNLIB F102
C
C     ref. J.H.Wilkinson & C.Reinsch
C          Linear Algebra - Springer-Verlag 1971
C          pag. 9 and 50
C
C
C     ******************************************************************
C
          DOUBLE PRECISION    A(IDIM,*), B(IDIM,*),  ONE,  X, Y
          REAL                PIVOTF
          DOUBLE PRECISION    S1, S21, S22,       DOTF
          CHARACTER*6         HNAME
          DATA      HNAME               /  'DSBEQN'  /
          PIVOTF(X)    =  SNGL(X)
          DOTF(X,Y,S1)  =  X * Y + S1
          DATA      ONE                 /  1.D0  /
          IF(IDIM .LT. N  .OR.  N .LE. 0  .OR.  K .LT. 0)  GOTO 900
          IFAIL  =  0
          DO 144    J  =  1, N
             IF(PIVOTF(A(J,J)) .LE. 0.)  GOTO 150
             A(J,J)  =  ONE / A(J,J)
             IF(J .EQ. N)  GOTO 199
 140         JP1  =  J+1
             JPM = MIN(J+M,N)
             JP1MM =MAX(1,JP1-M)
             DO 143   L  =  JP1, JPM
                A(J,L)  =  A(J,J)*A(L,J)
                S1      =  -A(L,JP1)
                IM = MAX(JP1MM,L-M)
                DO 141  I  =  IM, J
                   S1  =  DOTF(A(L,I),A(I,J+1),S1)
 141               CONTINUE
                A(L,JP1)  =  -S1
 143            CONTINUE
 144         CONTINUE
 150      IFAIL  =  -1
          RETURN
 199      CONTINUE
          IF(K .LE. 0)  GOTO 299
          DO 220    L  =  1, K
             B(1,L)  =  A(1,1)*B(1,L)
 220         CONTINUE
          IF(N .EQ. 1)  GOTO 299
          DO 243    L  =  1, K
             DO 232   I  =  2, N
                IM1  =  I-1
                IMM  =  MAX(1,I-M)
                S21  =  - B(I,L)
                DO 231   J  =  IMM, IM1
                   S21  =  DOTF(A(I,J),B(J,L),S21)
 231               CONTINUE
                B(I,L)  =  - A(I,I)*S21
 232            CONTINUE
             NM1  =  N-1
             DO 242   I  =  NM1,1,-1
                S22  =  - B(I,L)
                IP1 = I+1
                IPM = MIN(N,I+M)
                DO 241   J  =  IP1, IPM
                   S22    =  DOTF(A(I,J),B(J,L),S22)
 241               CONTINUE
                B(I,L)  =  - S22
 242            CONTINUE
 243         CONTINUE
 299      CONTINUE
          RETURN
 900      CALL TMPRNT(HNAME,N,IDIM,K)
          RETURN
          END
