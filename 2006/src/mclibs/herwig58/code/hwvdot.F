*
* $Id: hwvdot.F,v 1.1.1.1 1996/03/08 17:02:18 mclareni Exp $
*
* $Log: hwvdot.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:18  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.49  by  Unknown
*-- Author :
CDECK  ID>, HWVDOT.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      FUNCTION HWVDOT(N,P,Q)
      DOUBLE PRECISION HWVDOT
C---VECTOR DOT PRODUCT
      INTEGER N,I
      DOUBLE PRECISION PQ,P(N),Q(N)
      PQ=0.
      DO 10 I=1,N
   10 PQ=PQ+P(I)*Q(I)
      HWVDOT=PQ
      END
