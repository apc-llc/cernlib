*
* $Id: hwbsu2.F,v 1.1.1.1 1996/03/08 17:02:10 mclareni Exp $
*
* $Log: hwbsu2.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:10  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.46  by  Unknown
*-- Author :
CDECK  ID>, HWBSU2.
*CMZ :-        -13/07/92  20.15.54  by  Mike Seymour
*-- Author :    Bryan Webber, modified by Mike Seymour
C-----------------------------------------------------------------------
      FUNCTION HWBSU2(Z)
C     INTEGRAND IN EXPONENT OF QUARK SUDAKOV FORM FACTOR.
C     HWBSU2 IS FOR LOWER PART OF Z INTEGRATION REGION
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWBSU2,HWBSUL,Z,U
      U=1.-Z
      HWBSU2=HWBSUL(Z)*(1.+Z*Z)/U
      END
