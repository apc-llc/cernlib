*
* $Id: hwvzro.F,v 1.1.1.1 1996/03/08 17:02:19 mclareni Exp $
*
* $Log: hwvzro.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:19  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.49  by  Unknown
*-- Author :
CDECK  ID>, HWVZRO.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      SUBROUTINE HWVZRO(N,P)
C---ZERO VECTOR
      INTEGER N,I
      DOUBLE PRECISION P(N)
      DO 10 I=1,N
   10 P(I)=0.
      END
