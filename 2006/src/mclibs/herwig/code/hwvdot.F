CDECK  ID>, HWVDOT.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWVDOT(N,P,Q)
C-----------------------------------------------------------------------
C     VECTOR DOT PRODUCT
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWVDOT,PQ,P(N),Q(N)
      INTEGER N,I
      PQ=0.
      DO 10 I=1,N
   10 PQ=PQ+P(I)*Q(I)
      HWVDOT=PQ
      END
