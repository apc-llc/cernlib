*
* $Id: hwbsug.F,v 1.1.1.1 1996/03/08 17:02:10 mclareni Exp $
*
* $Log: hwbsug.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:10  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.46  by  Unknown
*-- Author :
CDECK  ID>, HWBSUG.
*CMZ :-        -13/07/92  20.15.54  by  Mike Seymour
*-- Author :    Bryan Webber, modified by Mike Seymour
C-----------------------------------------------------------------------
      FUNCTION HWBSUG(ZLOG)
C     Z TIMES INTEGRAND IN EXPONENT OF GLUON SUDAKOV FORM FACTOR
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWBSUG,HWBSUL,Z,ZLOG,W
      Z=EXP(ZLOG)
      W=Z*(1.-Z)
      HWBSUG=HWBSUL(Z)*(W-2.+1./W)*Z
      END
