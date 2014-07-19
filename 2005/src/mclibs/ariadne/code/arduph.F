C***********************************************************************
C $Id: arduph.F,v 1.2 1996/04/10 12:33:09 mclareni Exp $

      SUBROUTINE ARDUPH

C...ARiadne subroutine DUmp PHoton

C...Moves photon emitted by Ariadne to /LUJETS/

#include "arimpl.f"
#include "arpart.f"
#include "arstrs.f"
#include "arint3.f"
#include "lujets.f"


      N=N+1
      DO 100 I=1,5
        P(N,I)=BP(IPART+1,I)
        DPTOT(I)=DPTOT(I)-BP(IPART+1,I)
        V(N,I)=V(IMF,I)
 100  CONTINUE

      DPTOT(5)=DSQRT(DPTOT(4)**2-DPTOT(3)**2-DPTOT(2)**2-DPTOT(1)**2)

      K(N,1)=1
      K(N,2)=22
      K(N,3)=IMF
      K(N,4)=0
      K(N,5)=IO

      RETURN

C**** END OF ARDUPH ****************************************************
      END
