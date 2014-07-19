CDECK  ID>, HWUECM.
*CMZ :-        -29/01/93  11.11.55  by  Bryan Webber
*-- Author :    Giovanni Abbiendi & Luca Stanco
C---------------------------------------------------------------------
      FUNCTION HWUECM (S,M1QUAD,M2QUAD)
C-----------------------------------------------------------------------
C     C.M. ENERGY OF A PARTICLE IN 1-->2 BRANCH, MAY BE SPACELIKE
C---------------------------------------------------------------------
      DOUBLE PRECISION HWUECM,S,M1QUAD,M2QUAD
      HWUECM = (S+M1QUAD-M2QUAD)/(2.D0*SQRT(S))
      END
