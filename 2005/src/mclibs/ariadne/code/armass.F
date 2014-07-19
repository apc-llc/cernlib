C***********************************************************************
C $Id: armass.F,v 1.2 1996/04/10 12:33:22 mclareni Exp $

      REAL FUNCTION ARMAS2(I1,I2)

C...ARiadne function invariant MASs of 2 partons

C...Returns the invariant mass^2 of partons I1 and I2

      DIMENSION I(2)


      I(1)=I1
      I(2)=I2

      ARMAS2=ARMASS(2,I)

      RETURN

C**** END OF ARMAS2 ****************************************************
      END
C***********************************************************************
C $Id: armass.F,v 1.2 1996/04/10 12:33:22 mclareni Exp $

      REAL FUNCTION ARMAS3(I1,I2,I3)

C...ARiadne function invariant MASs of 3 partons

C...Returns the invariant mass^2 of partons I1, I2 and I3

      DIMENSION I(3)


      I(1)=I1
      I(2)=I2
      I(3)=I3
      
      ARMAS3=ARMASS(3,I)

      RETURN

C**** END OF ARMAS3 ****************************************************
      END
C***********************************************************************
C $Id: armass.F,v 1.2 1996/04/10 12:33:22 mclareni Exp $

      REAL FUNCTION ARMAS4(I1,I2,I3,I4)

C...ARiadne function invariant MASs of 4 partons

C...Returns the invariant mass^2 of partons I1, I2, I3 and I4

      DIMENSION I(4)


      I(1)=I1
      I(2)=I2
      I(3)=I3
      I(4)=I4

      ARMAS4=ARMASS(4,I)

      RETURN

C**** END OF ARMAS4 ****************************************************
      END
C***********************************************************************
C $Id: armass.F,v 1.2 1996/04/10 12:33:22 mclareni Exp $

      REAL FUNCTION ARMASS(N,I)

C...ARiadne function invariant MASS of partons

C...Returns the total invariant mass^2 of N partons

#include "arimpl.f"
#include "arpart.f"
      DIMENSION I(N),DPS(4)


      DO 100 IK=1,4
        DPS(IK)=0.0D0
        DO 200 IJ=1,N
          DPS(IK)=DPS(IK)+BP(I(IJ),IK)
 200    CONTINUE
 100  CONTINUE

      DMASS=DPS(4)**2-DPS(3)**2-DPS(2)**2-DPS(1)**2
      ARMASS=MAX(DMASS,0.0D0)

      RETURN

C**** END OF ARMASS ****************************************************
      END
