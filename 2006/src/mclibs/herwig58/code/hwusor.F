*
* $Id: hwusor.F,v 1.1.1.1 1996/03/08 17:02:18 mclareni Exp $
*
* $Log: hwusor.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:18  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.49  by  Unknown
*-- Author :
CDECK  ID>, HWUSOR.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Adapted by Bryan Webber
C------------------------------------------------------------------------
      SUBROUTINE HWUSOR(A,N,K,IOPT)
C     Sort A(N) into ascending order
C     IOPT = 1 : return sorted A and index array K
C     IOPT = 2 : return index array K only
C------------------------------------------------------------------------
      INTEGER N,I,J,IOPT,K(N),IL(500),IR(500)
      DOUBLE PRECISION A(N),B(500)
      IF (N.GT.500) CALL HWWARN('HWUSOR',100,*999)
      IL(1)=0
      IR(1)=0
      DO 10 I=2,N
      IL(I)=0
      IR(I)=0
      J=1
   2  IF(A(I).GT.A(J)) GO TO 5
   3  IF(IL(J).EQ.0) GO TO 4
      J=IL(J)
      GO TO 2
   4  IR(I)=-J
      IL(J)=I
      GO TO 10
   5  IF(IR(J).LE.0) GO TO 6
      J=IR(J)
      GO TO 2
   6  IR(I)=IR(J)
      IR(J)=I
  10  CONTINUE
      I=1
      J=1
      GO TO 8
  20  J=IL(J)
   8  IF(IL(J).GT.0) GO TO 20
   9  K(I)=J
      B(I)=A(J)
      I=I+1
      IF(IR(J)) 12,30,13
  13  J=IR(J)
      GO TO 8
  12  J=-IR(J)
      GO TO 9
  30  IF(IOPT.EQ.2) RETURN
      DO 31 I=1,N
  31  A(I)=B(I)
 999  END
