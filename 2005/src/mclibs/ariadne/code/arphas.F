C***********************************************************************
C $Id: arphas.F,v 1.2 1996/04/10 12:33:28 mclareni Exp $

      SUBROUTINE ARPHAS(IFIRST)

C...ARiadne function PHi ASymmetry

C...Calculate a phi-angle to rotate en emission to  correctly treat
C...asymmetries in O(alpha_S)  ME for lepto production.

#include "arimpl.f"
#include "ardat1.f"
#include "leptou.f"
#include "lujets.f"
#include "arint4.f"
#include "ludat1.f"


      IF (IFLASS.EQ.0) RETURN

      PHI=ULANGL(SNGL(BASS(1)),SNGL(BASS(2)))
      CALL LUDBRB(IFIRST,N,0.0,-PHI,0.0D0,0.0D0,0.0D0)


      BEPS1=1.0-PARA(39)
      BEPS0=PARA(39)
      B1=MIN(BASSX1,BEPS1)
      B3=MIN(BASSX3,BEPS1)
      B2=MIN(2.0D0-B1-B3,BEPS1)
      B12=1.0D0-B2

      SQ2=XQ2/W2
      XP=MAX(MIN(SQ2/(1.0-B3+SQ2),BEPS1),BEPS0)
      ZQ=MAX(MIN(B12/B3,BEPS1),BEPS0)


      IF (IFLASS.EQ.21) THEN
C.......Calculate guon asymmetry
        LST(24)=2
        SIGT=(ZQ**2+XP**2)/((1.0-XP)*(1.0-ZQ))+2.0*(XP*ZQ+1.0)
        SIGS=4.0*XP*ZQ
        SIG1=2.0*XY*SQRT((1.0-XY)*XP*ZQ/((1.0-XP)*(1.0-ZQ)))*
     $       (1.0-2.0/XY)*(1.0-ZQ-XP+2.0*XP*ZQ)
        SIG2=2.0*(1.0-XY)*XP*ZQ
      ELSE
C.......Calculate quark asymmetry
        LST(24)=3
        SIGT=(XP**2+(1.0-XP)**2)*(ZQ**2+(1.0-ZQ)**2)/(ZQ*(1.0-ZQ))
        SIGS=8.0*XP*(1.0-XP)
        SIG1=2.0*XY*SQRT((1.0-XY)*XP*(1.0-XP)/(ZQ*(1.0-ZQ)))
        SIG2=4.0*(1.0-XY)*XP*(1.0-XP)
      ENDIF

      SIG0=0.5*(1.0+(1.0-XY)**2)*SIGT+(1.0-XY)*SIGS

 100  PHI=RLU(0)*PARU(2)
      IF (SIG0+SIN(PHI)*SIG1+SIN(2.0*PHI)*SIG2.LT.
     $     RLU(0)*(SIG0+SIG1+SIG2)) GOTO 100

      CALL LUDBRB(IFIRST,N,0.0,PHI,0.0D0,0.0D0,0.0D0)
      
      RETURN

C**** END OF ARPHAS ****************************************************
      END
