CDECK  ID>, HWVDIF.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWVDIF(N,P,Q,R)
C-----------------------------------------------------------------------
C     VECTOR DIFFERENCE
C-----------------------------------------------------------------------
      DOUBLE PRECISION P(N),Q(N),R(N)
      INTEGER N,I
      DO 10 I=1,N
   10 R(I)=P(I)-Q(I)
      END
