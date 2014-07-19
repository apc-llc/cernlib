*
* $Id: bwmass.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: bwmass.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION BWMASS(IP)
C.----------------------------------------------------------------------
C.
C.    CALCULATE PARTICLE MASS ACCORDING TO TRUNCATED BREIT-WIGNER.
C.    LAST UPDATE: 10/04/89
C.
C.----------------------------------------------------------------------
#include "eurodec/ptable.inc"
#include "eurodec/pwidth.inc"
      IF (VMUP(IP).GT.0.) THEN
        BWMASS=SQRT(PM(IP)*(PM(IP)+GAM(IP)*TAN((VMUP(IP)-VMLO(IP))*
     &  EURRAN(IDUM)+VMLO(IP))))
      ELSE
        BWMASS=PM(IP)
      ENDIF
      RETURN
      END
