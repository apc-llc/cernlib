CDECK  ID>, HWRLOG.
*CMZ :-        -26/04/91  14.15.56  by  Federico Carminati
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWRLOG(A)
C-----------------------------------------------------------------------
C     Returns .TRUE. with probability A
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWRGEN,A,R
      LOGICAL HWRLOG
      EXTERNAL HWRGEN
      HWRLOG=.TRUE.
      R=HWRGEN(0)
      IF(R.GT.A) HWRLOG=.FALSE.
      END
