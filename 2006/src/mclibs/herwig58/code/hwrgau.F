*
* $Id: hwrgau.F,v 1.1.1.1 1996/03/08 17:02:16 mclareni Exp $
*
* $Log: hwrgau.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:16  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWRGAU.
*CMZ :-        -16/10/93  11.11.56  by  Mike Seymour
*-- Author :    Mike Seymour
C-----------------------------------------------------------------------
      FUNCTION HWRGAU(J,A,B)
      DOUBLE PRECISION HWRGAU
C ... Gaussian random number, mean A, standard deviation B.
C     Generates uncorrelated pairs and caches one of them.
      INTEGER J
      DOUBLE PRECISION A,B,X,HWRGEN,CACHE,EMPTY
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
