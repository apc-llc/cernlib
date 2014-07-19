CDECK  ID>, HWUROB.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWUROB(R,P,Q)
C-----------------------------------------------------------------------
C     ROTATES VECTORS BY INVERSE OF ROTATION MATRIX R
C-----------------------------------------------------------------------
      DOUBLE PRECISION S1,S2,S3,R(3,3),P(3),Q(3)
      S1=P(1)*R(1,1)+P(2)*R(2,1)+P(3)*R(3,1)
      S2=P(1)*R(1,2)+P(2)*R(2,2)+P(3)*R(3,2)
      S3=P(1)*R(1,3)+P(2)*R(2,3)+P(3)*R(3,3)
      Q(1)=S1
      Q(2)=S2
      Q(3)=S3
      END
