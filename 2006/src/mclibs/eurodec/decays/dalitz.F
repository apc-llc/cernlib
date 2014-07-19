*
* $Id: dalitz.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: dalitz.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION DALITZ(IP)
C.----------------------------------------------------------------------
C.
C.    MATRIX ELEMENT FOR PI0, ETA ---> GAMMA E+ E- DECAYS  (MATRIX = 1)
C.    LAST UPDATE: 14/08/89
C.
C.----------------------------------------------------------------------
#include "eurodec/result.inc"
      NTP1=NTEIL+1
      NTP2=NTEIL+2
      NTP3=NTEIL+3
      DOT12=DOT(NTP1,NTP2)
      DOT13=DOT(NTP1,NTP3)
      DOT23=DOT(NTP2,NTP3)
      AVIR2=2.*(DOT23+PTEIL(5,NTP2)**2)
      DALITZ=(8.*DOT23*(DOT12**2+DOT13**2)+16.*PTEIL(5,NTP2)**2*(DOT12*
     &DOT13+DOT12**2+DOT13**2))/(AVIR2*(PTEIL(5,IP)**2-AVIR2)**2)
      RETURN
      END
