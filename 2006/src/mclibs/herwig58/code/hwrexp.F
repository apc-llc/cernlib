*
* $Id: hwrexp.F,v 1.1.1.1 1996/03/08 17:02:16 mclareni Exp $
*
* $Log: hwrexp.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:16  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWREXP.
*CMZ :-        -26/04/91  11.11.55  by  Bryan Webber
*-- Author :    David Ward, modified by Bryan Webber
C------------------------------------------------------------------------
      FUNCTION HWREXP(AV)
      DOUBLE PRECISION HWREXP
C ... Random number from dN/d(x**2)=exp(-b*x) with mean AV
      DOUBLE PRECISION HWRGEN,AV,B,R1,R2
      B=2./AV
      R1=HWRGEN(0)
      R2=HWRGEN(1)
      HWREXP=-LOG(R1*R2)/B
      END
