CDECK  ID>, HWUSQR.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWUSQR(X)
C-----------------------------------------------------------------------
C     SQUARE ROOT WITH SIGN RETENTION
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWUSQR,X
      HWUSQR=SIGN(SQRT(ABS(X)),X)
      END
