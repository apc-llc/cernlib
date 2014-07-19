*
* $Id: ampvec.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: ampvec.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION AMPVEC(IP)
C.----------------------------------------------------------------------
C.
C.    MATRIX ELEMENT WEAK (VECTOR COUPLING) DECAYS   (MATRIX = 23 / 53)
C.    LAST UPDATE: 18/09/88
C.
C.----------------------------------------------------------------------
#include "eurodec/result.inc"
#include "eurodec/wekpar.inc"
C--
C--   DENOMINATOR OF BREIT-WIGNER FUNCTION FOR W PROPAGATOR
      BRWIG(Q2)=(Q2-AMW**2)**2+(AMW*GAMW)**2
      NTP1=NTEIL+1
      NTP3=NTEIL+3
      AMP2=PTEIL(5,IP)**2
      AM12=PTEIL(5,NTP1)**2
C--
C--   CALCULATE ACTUAL AND MAXIMAL BREIT-WIGNER VALUES
      WPROP=BRWIG(AMP2+AM12-2.*PTEIL(5,IP)*PTEIL(4,NTP1))
      WPROPM=BRWIG(MIN(AMP2+AM12-2.*PTEIL(5,IP)*PTEIL(5,NTP1),AMW**2))
      AMPVEC=PTEIL(4,NTP3)*(AMP2-AM12-2.*PTEIL(5,IP)*PTEIL(4,NTP3))
     &*(AMP2+2.*(AM12-PTEIL(5,IP)*PTEIL(4,NTP3)))/(PTEIL(5,IP)-2.*PTEIL
     &(4,NTP3))/(AMP2)**2
      AMPVEC=8.*AMPVEC*WPROPM/WPROP/1.1
      RETURN
      END
