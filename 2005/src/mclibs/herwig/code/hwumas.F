CDECK  ID>, HWUMAS.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWUMAS(P)
C-----------------------------------------------------------------------
C     PUTS INVARIANT MASS IN 5TH COMPONENT OF VECTOR
C     (NEGATIVE SIGN IF SPACELIKE)
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWUSQR,P(5)
      EXTERNAL HWUSQR
      P(5)=HWUSQR((P(4)+P(3))*(P(4)-P(3))-P(1)**2-P(2)**2)
      END
