CDECK  ID>, HWVZRO.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWVZRO(N,P)
C-----------------------------------------------------------------------
C     ZERO VECTOR
C-----------------------------------------------------------------------
      DOUBLE PRECISION P(N)
      INTEGER N,I
      DO 10 I=1,N
   10 P(I)=0.
      END
