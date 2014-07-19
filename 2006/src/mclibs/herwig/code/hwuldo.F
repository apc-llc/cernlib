CDECK  ID>, HWULDO.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C----------------------------------------------------------------------
      FUNCTION HWULDO(P,Q)
C----------------------------------------------------------------------
C   LORENTZ 4-VECTOR DOT PRODUCT
C----------------------------------------------------------------------
      DOUBLE PRECISION HWULDO,P(4),Q(4)
      HWULDO=P(4)*Q(4)-(P(1)*Q(1)+P(2)*Q(2)+P(3)*Q(3))
      END
