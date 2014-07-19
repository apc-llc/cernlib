CDECK  ID>, HWRUNI.
*CMZ :-        -26/04/91  14.55.45  by  Federico Carminati
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWRUNI(I,A,B)
C-----------------------------------------------------------------------
C     Uniform random random number in range [A,B]
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWRUNI,HWRGEN,A,B,RN
      INTEGER I
      EXTERNAL HWRGEN
      RN=HWRGEN(I)
      HWRUNI=A+RN*(B-A)
      END
