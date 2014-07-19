CDECK  ID>, HWBSU2.
*CMZ :-        -13/07/92  20.15.54  by  Mike Seymour
*-- Author :    Bryan Webber, modified by Mike Seymour
C-----------------------------------------------------------------------
      FUNCTION HWBSU2(Z)
C-----------------------------------------------------------------------
C     INTEGRAND IN EXPONENT OF QUARK SUDAKOV FORM FACTOR.
C     HWBSU2 IS FOR LOWER PART OF Z INTEGRATION REGION
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWBSU2,HWBSUL,Z,U
      EXTERNAL HWBSUL
      U=1.-Z
      HWBSU2=HWBSUL(Z)*(1.+Z*Z)/U
      END
