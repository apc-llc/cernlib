CDECK  ID>, HWBSU1.
*CMZ :-        -13/07/92  20.15.54  by  Mike Seymour
*-- Author :    Bryan Webber, modified by Mike Seymour
C-----------------------------------------------------------------------
      FUNCTION HWBSU1(ZLOG)
C-----------------------------------------------------------------------
C     Z TIMES THE INTEGRAND IN EXPONENT OF QUARK SUDAKOV FORM FACTOR.
C     HWBSU1 IS FOR UPPER PART OF Z INTEGRATION REGION
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWBSU1,HWBSUL,Z,ZLOG,U
      EXTERNAL HWBSUL
      Z=EXP(ZLOG)
      U=1.-Z
      HWBSU1=HWBSUL(Z)*(1.+U*U)
      END
