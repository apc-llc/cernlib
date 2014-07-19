CDECK  ID>, HWBSUG.
*CMZ :-        -13/07/92  20.15.54  by  Mike Seymour
*-- Author :    Bryan Webber, modified by Mike Seymour
C-----------------------------------------------------------------------
      FUNCTION HWBSUG(ZLOG)
C-----------------------------------------------------------------------
C     Z TIMES INTEGRAND IN EXPONENT OF GLUON SUDAKOV FORM FACTOR
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWBSUG,HWBSUL,Z,ZLOG,W
      EXTERNAL HWBSUL
      Z=EXP(ZLOG)
      W=Z*(1.-Z)
      HWBSUG=HWBSUL(Z)*(W-2.+1./W)*Z
      END
