*
* $Id: dsbfinv.F,v 1.1.1.1 1996/03/06 15:36:26 mclareni Exp $
*
* $Log: dsbfinv.F,v $
* Revision 1.1.1.1  1996/03/06 15:36:26  mclareni
* Add geane321 examples
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.50  by  S.Giani
*-- Author :
          SUBROUTINE          DSBFINV(N,M,A,IDIM,LB,B,LST)
C
C     ******************************************************************
C
C     INVERT A BANDED MATRIX USING CHOLESKY DECOMPOSITION
C     ROUTINE DSBEQN OR SIMILAR SHOULD BE CALLED FIRST
C     THE ELEMENT OF THE INVERTED MATRIX TO BE COMPIUTED ARE LISTED
C     IN LST AND STORED IN B
C
C     N       ORDER OF THE BAND MATRIX.
C
C     M       BAND PARAMETER. NON-ZERO COEFFICIENTS ARE CONFINED TO
C             IABS(I-J).LE.M.
C
C     A       (DOUBLE PRECISION) TWO-DIMENSIONAL ARRAY CONTAINING THE
C             SUCCESSIVE ROWS OF THE CHOLESKY DECOMPOSED BAND MATRIX
C
C     IDIM    FIRST DIMENSION PARAMETER OF ARRAYS ABAND AND B.
C
C     LB       NUMBER OF ELEMENTS TO BE INVERTED
C
C     B       (DOUBLE PRECISION) ARRAY CONTAINING IN EXIT THE INVERTED
C             ELEMENTS
C
C     LST     TWO-DIMENTIONAL ARRAY CONTEINING THE INDECES OF THE
C             ELEMENT OF THE MATRIX TO BE INVERTED
C
C                                                            -1
C     THIS SUBROUTINE REPLACES B(J)  BY  A(LST(1,J),LST(2,J))
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
          DOUBLE PRECISION    A(IDIM,*),ZERO,     X, Y
          DOUBLE PRECISION    B(*)
          DOUBLE PRECISION    S31, S32, S33, DOTF
          INTEGER             LST(2,*)
          CHARACTER*7         HNAME
          DOTF(X,Y,S31)  =  X*Y + S31
          DATA      HNAME               /  'DSBFINV'  /
          DATA      ZERO      /  0.D0  /
          IF(IDIM .LT. N  .OR.  N .LE. 0)  GOTO 900
          IF(N .EQ. 1)  GOTO 399
          A(1,2)  =  -A(1,2)
          A(2,1)  =   A(1,2)*A(2,2)
          IF(N .EQ. 2)  GOTO 320
          DO 314    J  =  3, N
             JM2  =  J - 2
             JMM = MAX(1,J-1-M)
             DO 312 K  =  1, JM2
                S31  =  A(K,J)
                IM = MAX(K,JMM)
                DO 311  I  =  IM, JM2
                   S31  =  DOTF(A(K,I+1),A(I+1,J),S31)
 311               CONTINUE
                A(K,J)  =  -S31
                A(J,K)  =  -S31*A(J,J)
 312            CONTINUE
             A(J-1,J)  =  -A(J-1,J)
             A(J,J-1)  =   A(J-1,J)*A(J,J)
 314         CONTINUE
*
 320         CONTINUE
             IF ( LB.LE.0 ) GO TO 999
             DO 329 L=1,LB
               K  =  LST(1,L)
               J  =  LST(2,L)
               IF (J.EQ.K)  THEN
                 S33  =  A(J,J)
                 IF(J .EQ. N)  GOTO 325
                 JP1  =  J + 1
                 DO 324 I  =  JP1, N
                   S33  =  DOTF(A(J,I),A(I,J),S33)
 324            CONTINUE
 325            B(L)  =  S33
               ELSE
                 IF ( K.GT.J ) THEN
                  K  =  LST(2,L)
                  J  =  LST(1,L)
                 ENDIF
                 S32  =  ZERO
                 DO 327  I  =  J, N
                   S32  =  DOTF(A(K,I),A(I,J),S32)
 327             CONTINUE
                 B(L)  =  S32
               ENDIF
 329         CONTINUE
 399      CONTINUE
          RETURN
 900      CALL TMPRNT(HNAME,N,IDIM,0)
 999      CONTINUE
          RETURN
          END
 
