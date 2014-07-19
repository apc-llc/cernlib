CDECK  ID>, HWVEQU.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWVEQU(N,P,Q)
C-----------------------------------------------------------------------
C     VECTOR EQUALITY
C-----------------------------------------------------------------------
      DOUBLE PRECISION P(N),Q(N)
      INTEGER N,I
      DO 10 I=1,N
   10 Q(I)=P(I)
      END
