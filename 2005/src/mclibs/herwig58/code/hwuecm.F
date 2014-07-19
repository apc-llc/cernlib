*
* $Id: hwuecm.F,v 1.1.1.1 1996/03/08 17:02:17 mclareni Exp $
*
* $Log: hwuecm.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:17  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWUECM.
*CMZ :-        -29/01/93  11.11.55  by  Bryan Webber
*-- Author :    Giovanni Abbiendi & Luca Stanco
C---------------------------------------------------------------------
      FUNCTION HWUECM (S,M1QUAD,M2QUAD)
C-- C.M. ENERGY OF A PARTICLE IN 1-->2 BRANCH, MAY BE SPACELIKE
      DOUBLE PRECISION HWUECM,S,M1QUAD,M2QUAD
      HWUECM = (S+M1QUAD-M2QUAD)/(2.D0*SQRT(S))
      END
