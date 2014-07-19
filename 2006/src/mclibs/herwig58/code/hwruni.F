*
* $Id: hwruni.F,v 1.1.1.1 1996/03/08 17:02:16 mclareni Exp $
*
* $Log: hwruni.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:16  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWRUNI.
*CMZ :-        -26/04/91  14.55.45  by  Federico Carminati
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      FUNCTION HWRUNI(I,A,B)
      DOUBLE PRECISION HWRUNI
C ... Uniform random random number in range [A,B]
      INTEGER I
      DOUBLE PRECISION HWRGEN,A,B,RN
      RN=HWRGEN(I)
      HWRUNI=A+RN*(B-A)
      END
