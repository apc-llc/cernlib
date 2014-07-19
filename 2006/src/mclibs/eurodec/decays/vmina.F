*
* $Id: vmina.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: vmina.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION VMINA(IP)
C.----------------------------------------------------------------------
C.
C.    MATRIX ELEMENT FOR WEAK (V - A) DECAYS         (MATRIX = 22 / 52)
C.    LAST UPDATE: 01/03/89
C.
C.----------------------------------------------------------------------
#include "eurodec/result.inc"
#include "eurodec/wekpar.inc"
C--
C--   DENOMINATOR OF BREIT-WIGNER FUNCTION FOR W PROPAGATOR
      BRWIG(Q2)=(Q2-AMW**2)**2+(AMW*GAMW)**2
      NTP1=NTEIL+1
      NTP2=NTEIL+2
      AMP2=PTEIL(5,IP)**2
      AM12=PTEIL(5,NTP1)**2
      DOT13=DOT(NTP1,NTEIL+3)
C--
C--   CALCULATE ACTUAL AND MAXIMAL BREIT-WIGNER VALUES
      WPROP=BRWIG(AMP2+AM12-2.*PTEIL(5,IP)*PTEIL(4,NTP1))
      WPROPM=BRWIG(MIN(AMP2+AM12-2.*PTEIL(5,IP)*PTEIL(5,NTP1),AMW**2))
      IF (ITHEL(IP).NE.0) THEN
        PP=SQRT((PTEIL(4,IP)-PTEIL(5,IP))*(PTEIL(4,IP)+PTEIL(5,IP)))
        P2=SQRT((PTEIL(4,NTP2)-PTEIL(5,NTP2))*(PTEIL(4,NTP2)+PTEIL
     &  (5,NTP2)))
        COST=(PTEIL(1,IP)*PTEIL(1,NTP2)+PTEIL(2,IP)*PTEIL(2,NTP2)+PTEIL
     &  (3,IP)*PTEIL(3,NTP2))/(PP*P2)
        VMINA=DOT13*PTEIL(5,IP)*(PTEIL(4,NTP2)+FLOAT(ITHEL(IP))*P2
     &  *COST)*WPROPM/WPROP/2.
      ELSE
        VMINA=DOT13*PTEIL(5,IP)*PTEIL(4,NTP2)*WPROPM/WPROP
      ENDIF
      VMINA=16.*VMINA/(AMP2**2)
      RETURN
      END
