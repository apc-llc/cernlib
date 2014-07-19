*
* $Id: fc346.F,v 1.1.1.1 1996/04/01 15:01:19 mclareni Exp $
*
* $Log: fc346.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:19  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      FUNCTION FC346(T)
#include "imp64r.inc"
      COMMON /PARAMT/ AKP,A,B,P,FMODE

      ENTRY F1EL1(T)
      F1EL1=1/SQRT((1+T**2)*(1+(AKP*T)**2))
      RETURN

      ENTRY F1EL2(T)
      F1EL2=(A+B*T**2)/((1+T**2)*SQRT((1+T**2)*(1+FMODE*(AKP*T)**2)))
      RETURN
      END
