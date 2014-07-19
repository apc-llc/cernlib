*
* $Id: tausca.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: tausca.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION TAUSCA(IP)
C.----------------------------------------------------------------------
C.
C.    MATRIX ELEMENT FOR POLARIZED TAU ---> TAUNEU + SCALAR,
C.    SCALAR = K, PI                                       (MATRIX = 4)
C.    LAST UPDATE: 01/03/89
C.
C.----------------------------------------------------------------------
#include "eurodec/result.inc"
      NTP1=NTEIL+1
      NTP2=NTEIL+2
      AMT2=PTEIL(5,IP)**2
      AMN2=PTEIL(5,NTP1)**2
      AMH2=PTEIL(5,NTP2)**2
      PTAU=SQRT((PTEIL(4,IP)-PTEIL(5,IP))*(PTEIL(4,IP)+PTEIL(5,IP)))
      PH=SQRT((PTEIL(4,NTP2)-PTEIL(5,NTP2))*(PTEIL(4,NTP2)+PTEIL
     &(5,NTP2)))
      COST=(PTEIL(1,IP)*PTEIL(1,NTP2)+PTEIL(2,IP)*PTEIL(2,NTP2)+PTEIL
     &(3,IP)*PTEIL(3,NTP2))/(PTAU*PH)
      TAUSCA=1.+(FLOAT(ITHEL(IP))*PH*COST*(AMT2-AMN2))/(PTEIL(4,NTP2)
     &*(AMT2-AMH2-AMN2)-PTEIL(4,NTP1)*AMH2)
      TAUSCA=TAUSCA/2.
      RETURN
      END
