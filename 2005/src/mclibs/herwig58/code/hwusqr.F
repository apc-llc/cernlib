*
* $Id: hwusqr.F,v 1.1.1.1 1996/03/08 17:02:18 mclareni Exp $
*
* $Log: hwusqr.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:18  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.49  by  Unknown
*-- Author :
CDECK  ID>, HWUSQR.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      FUNCTION HWUSQR(X)
      DOUBLE PRECISION HWUSQR
C     SQUARE ROOT WITH SIGN RETENTION
C------------------------------------------------------------------------
      DOUBLE PRECISION X
      HWUSQR=SIGN(SQRT(ABS(X)),X)
      END
