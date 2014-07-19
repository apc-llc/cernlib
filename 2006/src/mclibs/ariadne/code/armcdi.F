C***********************************************************************
C $Id: armcdi.F,v 1.2 1996/04/10 12:33:22 mclareni Exp $

      SUBROUTINE ARMCDI(ARRNDX,ARRNDY,ARVETO)

C...ARiadne subroutine Monte Carlo DIstribution

C...Generates x_1 and x_3 for a radiating dipole

#include "arimpl.f"
#include "arint1.f"


C...Exit if below cut
 100  IF (XT2M.LT.XT2C) GOTO 900
      QFAIL=.FALSE.

C...Generate random XT2
      XT2=ARRNDX()
      IF (XT2.LT.XT2C) GOTO 900
      XT=SQRT(XT2)

C...Generate rapidity Y
      Y=ARRNDY()

C...Calculate energy fractions
      B1=BC1-XT*EXP(Y)
      B3=BC3-XT*EXP(-Y)
      B2=2.0-B1-B3

C...Set maximum XT2 for possible next random call (VETO algorithm)
      XT2M=XT2

C...Redo random calls according to veto-algorithm
      IF (QFAIL.OR.ARVETO().LE.RLU(IDUM)) GOTO 100

C...Check that Current values are kinematically allowed
      CALL ARCHKI(0,IOK)
      IF (IOK.EQ.0) GOTO 100

      RETURN

C...If below cuts set XT2 to 0
 900  B1=BC1
      B3=BC3
      XT2=0.0

      RETURN

C**** END OF ARMCDI ****************************************************
      END
