*
* $Id: hwvdif.F,v 1.1.1.1 1996/03/08 17:02:18 mclareni Exp $
*
* $Log: hwvdif.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:18  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.49  by  Unknown
*-- Author :
CDECK  ID>, HWVDIF.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      SUBROUTINE HWVDIF(N,P,Q,R)
C---VECTOR DIFFERENCE
      INTEGER N,I
      DOUBLE PRECISION P(N),Q(N),R(N)
      DO 10 I=1,N
   10 R(I)=P(I)-Q(I)
      END
