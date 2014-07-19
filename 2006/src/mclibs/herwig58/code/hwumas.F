*
* $Id: hwumas.F,v 1.1.1.1 1996/03/08 17:02:18 mclareni Exp $
*
* $Log: hwumas.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:18  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.49  by  Unknown
*-- Author :
CDECK  ID>, HWUMAS.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      SUBROUTINE HWUMAS(P)
C     PUTS INVARIANT MASS IN 5TH COMPONENT OF VECTOR
C     (NEGATIVE SIGN IF SPACELIKE)
C------------------------------------------------------------------------
      DOUBLE PRECISION HWUSQR,P(5)
      P(5)=HWUSQR((P(4)+P(3))*(P(4)-P(3))-P(1)**2-P(2)**2)
      END
