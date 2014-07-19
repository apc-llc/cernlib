*
* $Id: hwvequ.F,v 1.1.1.1 1996/03/08 17:02:18 mclareni Exp $
*
* $Log: hwvequ.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:18  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.49  by  Unknown
*-- Author :
CDECK  ID>, HWVEQU.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      SUBROUTINE HWVEQU(N,P,Q)
C---VECTOR EQUALITY
      INTEGER N,I
      DOUBLE PRECISION P(N),Q(N)
      DO 10 I=1,N
   10 Q(I)=P(I)
      END
