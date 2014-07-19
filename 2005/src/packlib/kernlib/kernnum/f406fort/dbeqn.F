*
* $Id: dbeqn.F,v 1.1.1.1 1996/02/15 17:48:49 mclareni Exp $
*
* $Log: dbeqn.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:49  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
      SUBROUTINE DBEQN(NN,MM,ABAND,IDIM,IFAIL,KK,B)
      LOGICAL MFLAG,RFLAG
      DIMENSION ABAND(IDIM,NN),B(IDIM,KK)
      DOUBLE PRECISION ABAND,B,T,TMAX,P,ZERO
      DATA ZERO/0.D0/
C
C     ******************************************************************
C
C     SOLVES A BANDED SYSTEM OF LINEAR EQUATIONS USING GAUSSIAN ELIMINA-
C     TION WITH ROW INTERCHANGES.
C
C     NN      ORDER OF THE BAND MATRIX.
C
C     MM      BAND PARAMETER. NON-ZERO COEFFICIENTS ARE CONFINED TO
C             IABS(I-J).LE.MM.
C
C     ABAND   (DOUBLE PRECISION) TWO-DIMENSIONAL ARRAY CONTAINING THE
C             SUCCESSIVE ROWS OF THE BAND MATRIX WITH ITS OFF-BAND
C             ELEMENTS DELETED.
C
C     IDIM    FIRST DIMENSION PARAMETER OF ARRAYS ABAND AND B.
C
C     IFAIL   OUTPUT PARAMETER.   IFAIL= 0 ... NORMAL EXIT.
C                                 IFAIL=-1 ... SINGULAR MATRIX.
C
C     KK      NUMBER OF COLUMNS OF THE MATRIX IN ARRAY B.
C
C     B       (DOUBLE PRECISION) TWO-DIMENSIONAL ARRAY CONTAINING A
C             MATRIX OF RIGHT-HAND SIDES.
C
C     THIS SUBROUTINE REPLACES B BY THE SOLUTION MATRIX X OF A*X=B,
C     WHERE A IS THE BAND-MATRIX STORED IN PACKED FORM IN ARRAY ABAND.
C
C     CALLS ... ABEND.
C
C     ******************************************************************
C
C  START.  TEST INTEGER PARAMETERS.
   10 IFAIL=-1
      N=NN
      M=MM
      K=KK
      MBAND=2*M+1
      IF( (N.LT.1).OR.(N.GT.IDIM).OR.(M.LT.0).OR.(M.GE.N).OR.(K.LT.1) )
     *                                                         GO TO 100
C
C  SET ZEROS IN UPPER-RIGHT TRIANGLE OF ABAND.
      IF(M.EQ.0) GO TO 40
      JMAX=MIN0(MBAND,N)
      IMAX=JMAX-M-1
      JMIN=M+1
      IF(1.GT.IMAX) GO TO 20
      DO 12 I=1,IMAX
         JMIN=JMIN+1
         DO 11 J=JMIN,JMAX
            ABAND(I,J)=ZERO
   11    CONTINUE
   12 CONTINUE
C
C  GAUSSIAN ELIMINATION TO REDUCE MATRIX TO UPPER TRIANGULAR FORM.
C  (WITHIN THIS SECTION, IMAX=MIN(L+M,N), JMAX=MIN(N-L+1,MBAND).)
   20 IMAX=M
      LCUT=N-MBAND
      NMINUS=N-1
      DO 28 L=1,NMINUS
         LPLUS=L+1
         IF(IMAX.LT.N) IMAX=IMAX+1
C
C        PIVOT SEARCH. SET TMAX TO ABS(PIVOT).
         TMAX=DABS(ABAND(L,1))
         IPIV=L
         DO 21 I=LPLUS,IMAX
            T=DABS(ABAND(I,1))
            IF(T.LE.TMAX) GO TO 21
               TMAX=T
               IPIV=I
   21    CONTINUE
C
C        INTERCHANGE ROWS L AND IPIV.
         IF(IPIV.EQ.L) GO TO 24
         DO 22 J=1,JMAX
            T=ABAND(IPIV,J)
            ABAND(IPIV,J)=ABAND(L,J)
            ABAND(L,J)=T
   22    CONTINUE
         DO 23 JRHS=1,K
            T=B(IPIV,JRHS)
            B(IPIV,JRHS)=B(L,JRHS)
            B(L,JRHS)=T
   23    CONTINUE
C
C        ELIMINATE.
   24    P=ABAND(L,1)
         IF(P.EQ.ZERO) RETURN
         DO 27 I=LPLUS,IMAX
            T=ABAND(I,1)/P
            DO 25 J=2,JMAX
               ABAND(I,J-1)=ABAND(I,J)-T*ABAND(L,J)
   25       CONTINUE
            ABAND(I,JMAX)=ZERO
            DO 26 JRHS=1,K
               B(I,JRHS)=B(I,JRHS)-T*B(L,JRHS)
   26       CONTINUE
   27    CONTINUE
C
         IF(L.GT.LCUT) JMAX=JMAX-1
   28 CONTINUE
      IF(ABAND(N,1).EQ.ZERO) RETURN
C
C  BACK-SUBSTITUTION.
   30 DO 34 JRHS=1,K
         JMAX=0
         I=N
         DO 33 ICOMP=1,N
            IF(JMAX.LT.MBAND) JMAX=JMAX+1
            L=I
            T=B(I,JRHS)
            IF(2.GT.JMAX) GO TO 32
            DO 31 J=2,JMAX
               L=L+1
               T=T-ABAND(I,J)*B(L,JRHS)
   31       CONTINUE
   32       B(I,JRHS)=T/ABAND(I,1)
            I=I-1
   33    CONTINUE
   34 CONTINUE
      IFAIL=0
      RETURN
C
C  SPECIAL CASE M=0.
   40 DO 42 JRHS=1,K
         DO 41 I=1,N
            T=ABAND(I,1)
            IF(T.EQ.ZERO) RETURN
            B(I,JRHS)=B(I,JRHS)/T
   41    CONTINUE
   42 CONTINUE
      IFAIL=0
      RETURN
C
C  ERROR PRINT FOR PARAMETER ERROR.
  100 CALL KERMTR('F406.1',LGFILE,MFLAG,RFLAG)
      IF(MFLAG) THEN
         IF(LGFILE.EQ.0) THEN
            WRITE(*,101) N,M,IDIM,K
         ELSE
            WRITE(LGFILE,101) N,M,IDIM,K
         ENDIF
      ENDIF
      IF(.NOT. RFLAG) CALL ABEND
      RETURN
C
  101 FORMAT( 7X, 36HSUBROUTINE DBEQN ... PARAMETER ERROR,
     *        7X, 4HN = , I6, 7X, 4HM = , I6, 7X, 7HIDIM = , I6, 7X,
     *        4HK = , I6, 1H. )
      END
