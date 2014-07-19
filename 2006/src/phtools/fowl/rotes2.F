*
* $Id: rotes2.F,v 1.1.1.1 1996/03/22 16:42:47 mclareni Exp $
*
* $Log: rotes2.F,v $
* Revision 1.1.1.1  1996/03/22 16:42:47  mclareni
* Phtools
*
*
      SUBROUTINE ROTES2(C,S,C2,S2,PR,I)
C--  CALLED FROM - GENEV
C         THIS SUBROUTINE NOW DOES TWO ROTATIONS (XY AND XZ)
      DIMENSION PR(50)
      K1 = 5*I - 4
      K2 = K1 + 1
      SA = PR(K1)
      SB = PR(K2)
      A      = SA*C - SB*S
      PR(K2) = SA*S + SB*C
      K2 = K2 + 1
      B = PR(K2)
      PR(K1) = A*C2 - B*S2
      PR(K2) = A*S2 + B*C2
      RETURN
      END
