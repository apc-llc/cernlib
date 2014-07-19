
C ********************************************************************

      SUBROUTINE LECSWI(I,J)

C--       switch colour pointers for partons in rows I and J 
C--       and anticolour pointers for the partons pointing back

      IMPLICIT NONE

C--       global variables
      INTEGER N,K
      REAL P,V
      COMMON /LUJETS/N,K(4000,5),P(4000,5),V(4000,5)

C--       local variables
      INTEGER I,J,KI4,KJ4

      KI4=K(I,4)
      KJ4=K(J,4)
      K(KI4,5)=J
      K(KJ4,5)=I
      K(I,4)=KJ4
      K(J,4)=KI4

      END
