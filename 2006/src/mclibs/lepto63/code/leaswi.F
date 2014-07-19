
C ********************************************************************

      SUBROUTINE LEASWI(I,J)

C--       switch anticolour pointers for partons in rows I and J
C--       and colour pointers for the partons pointing back

      IMPLICIT NONE

C--       global variables
      INTEGER N,K
      REAL P,V
      COMMON /LUJETS/N,K(4000,5),P(4000,5),V(4000,5)

C--       local variables
      INTEGER I,J,KI5,KJ5

      KI5=K(I,5)
      KJ5=K(J,5)
      K(KI5,4)=J
      K(KJ5,4)=I
      K(I,5)=KJ5
      K(J,5)=KI5         

      END
