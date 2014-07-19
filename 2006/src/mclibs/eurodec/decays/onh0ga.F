*
* $Id: onh0ga.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: onh0ga.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION ONH0GA(IP)
C.----------------------------------------------------------------------
C.
C.    MATRIX ELEMENT FOR ONIUM ---> H0 GAMMA DECAYS       (MATRIX = 15)
C.    NOTE:  BEAM-AXIS ALONG X!
C.    LAST UPDATE: 20/03/89
C.
C.----------------------------------------------------------------------
#include "eurodec/result.inc"
      NTP2=NTEIL+2
      PGAMMA=SQRT((PTEIL(4,NTP2)-PTEIL(5,NTP2))*(PTEIL(4,NTP2)+PTEIL
     &(5,NTP2)))
      COST=PTEIL(1,NTP2)/PGAMMA
      ONH0GA=(1.+COST**2)/2.
      RETURN
      END
