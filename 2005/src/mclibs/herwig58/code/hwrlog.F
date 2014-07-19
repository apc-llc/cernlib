*
* $Id: hwrlog.F,v 1.1.1.1 1996/03/08 17:02:16 mclareni Exp $
*
* $Log: hwrlog.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:16  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWRLOG.
*CMZ :-        -26/04/91  14.15.56  by  Federico Carminati
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      LOGICAL FUNCTION HWRLOG(A)
C ... Returns .TRUE. with probability A
      DOUBLE PRECISION HWRGEN,A,R
      HWRLOG=.TRUE.
      R=HWRGEN(0)
      IF(R.GT.A) HWRLOG=.FALSE.
      END
