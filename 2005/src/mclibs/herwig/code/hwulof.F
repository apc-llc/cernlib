CDECK  ID>, HWULOF.
*CMZ :-        -05/11/95  19.33.42  by  Mike Seymour
*-- Author :    Adapted by Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWULOF(PS,PI,PF)
C-----------------------------------------------------------------------
C     TRANSFORMS PI (GIVEN IN LAB) INTO PF (IN REST FRAME OF PS)
C     N.B. P(1,2,3,4,5) = (PX,PY,PZ,E,M)
C-----------------------------------------------------------------------
      DOUBLE PRECISION PS(5),PI(5),PF(5)
      CALL HWULF4(PS,PI,PF)
      PF(5)= PI(5)
      END
