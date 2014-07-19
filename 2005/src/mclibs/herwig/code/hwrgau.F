CDECK  ID>, HWRGAU.
*CMZ :-        -16/10/93  11.11.56  by  Mike Seymour
*-- Author :    Mike Seymour
C-----------------------------------------------------------------------
      FUNCTION HWRGAU(J,A,B)
C-----------------------------------------------------------------------
C     Gaussian random number, mean A, standard deviation B.
C     Generates uncorrelated pairs and caches one of them.
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWRGAU,HWRGEN,A,B,X,CACHE,EMPTY
      INTEGER J
      EXTERNAL HWRGEN
      DATA CACHE,EMPTY/2*1D20/
      IF (CACHE.EQ.EMPTY) THEN
 10     X=HWRGEN(J)
        IF (X.LE.0.OR.X.GT.1) GOTO 10
        X=SQRT(-2*LOG(X))
        CALL HWRAZM(X,X,CACHE)
        HWRGAU=A+B*X
      ELSE
        HWRGAU=A+B*CACHE
        CACHE=EMPTY
      ENDIF
      END
