*
* $Id: hwurof.F,v 1.1.1.1 1996/03/08 17:02:18 mclareni Exp $
*
* $Log: hwurof.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:18  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.49  by  Unknown
*-- Author :
CDECK  ID>, HWUROF.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      SUBROUTINE HWUROF(R,P,Q)
C     ROTATES VECTORS BY ROTATION MATRIX R
C------------------------------------------------------------------------
      DOUBLE PRECISION S1,S2,S3,R(3,3),P(3),Q(3)
      S1=R(1,1)*P(1)+R(1,2)*P(2)+R(1,3)*P(3)
      S2=R(2,1)*P(1)+R(2,2)*P(2)+R(2,3)*P(3)
      S3=R(3,1)*P(1)+R(3,2)*P(2)+R(3,3)*P(3)
      Q(1)=S1
      Q(2)=S2
      Q(3)=S3
      END
