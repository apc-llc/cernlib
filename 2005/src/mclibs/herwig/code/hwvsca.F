CDECK  ID>, HWVSCA.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWVSCA(N,C,P,Q)
C-----------------------------------------------------------------------
C     VECTOR TIMES SCALAR
C-----------------------------------------------------------------------
      DOUBLE PRECISION C,P(N),Q(N)
      INTEGER N,I
      DO 10 I=1,N
   10 Q(I)=C*P(I)
      END
