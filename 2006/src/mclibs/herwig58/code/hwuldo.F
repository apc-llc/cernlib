*
* $Id: hwuldo.F,v 1.1.1.1 1996/03/08 17:02:17 mclareni Exp $
*
* $Log: hwuldo.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:17  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.49  by  Unknown
*-- Author :
CDECK  ID>, HWULDO.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C----------------------------------------------------------------------
      FUNCTION HWULDO(P,Q)
      DOUBLE PRECISION HWULDO
C---LORENTZ 4-VECTOR DOT PRODUCT
      DOUBLE PRECISION P(4),Q(4)
      HWULDO=P(4)*Q(4)-(P(1)*Q(1)+P(2)*Q(2)+P(3)*Q(3))
      END
