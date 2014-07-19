*
* $Id: hwurot.F,v 1.1.1.1 1996/03/08 17:02:18 mclareni Exp $
*
* $Log: hwurot.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:18  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.49  by  Unknown
*-- Author :
CDECK  ID>, HWUROT.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      SUBROUTINE HWUROT(P,CP,SP,R)
C     R IS ROTATION MATRIX TO GET FROM VECTOR P TO Z AXIS, FOLLOWED BY
C     A ROTATION BY PSI ABOUT Z AXIS, WHERE CP = COS-PSI, SP = SIN-PSI
C------------------------------------------------------------------------
      DOUBLE PRECISION WN,CP,SP,PTCUT,PP,PT,CT,ST,CF,SF,P(3),R(3,3)
      DATA WN,PTCUT/1.,1.D-20/
      PT=P(1)**2+P(2)**2
      PP=P(3)**2+PT
      IF (PT.LE.PP*PTCUT) THEN
         CT=SIGN(WN,P(3))
         ST=0.
         CF=1.
         SF=0.
      ELSE
         PP=SQRT(PP)
         PT=SQRT(PT)
         CT=P(3)/PP
         ST=PT/PP
         CF=P(1)/PT
         SF=P(2)/PT
      END IF
      R(1,1)= CP*CF*CT+SP*SF
      R(1,2)= CP*SF*CT-SP*CF
      R(1,3)=-CP*ST
      R(2,1)=-CP*SF+SP*CF*CT
      R(2,2)= CP*CF+SP*SF*CT
      R(2,3)=-SP*ST
      R(3,1)= CF*ST
      R(3,2)= SF*ST
      R(3,3)= CT
      END
