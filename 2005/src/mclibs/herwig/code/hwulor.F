CDECK  ID>, HWULOR.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Giovanni Abbiendi & Luca Stanco
C-----------------------------------------------------------------------
      SUBROUTINE HWULOR (TRANSF,PI,PF)
C-----------------------------------------------------------------------
C     Makes the HWULOR transformation specified by TRANSF on the
C     quadrivector PI(5), giving PF(5).
C-----------------------------------------------------------------------
      DOUBLE PRECISION TRANSF(4,4),PI(5),PF(5)
      INTEGER I,J
      DO 1 I=1,5
        PF(I)=0.D0
    1 CONTINUE
      DO 3 I=1,4
       DO 2 J=1,4
         PF(I) = PF(I) + TRANSF(I,J) * PI(J)
    2  CONTINUE
    3 CONTINUE
      PF(5) = PI(5)
      RETURN
      END
