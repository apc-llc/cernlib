*
* $Id: omephi.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: omephi.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION OMEPHI(IP)
C.----------------------------------------------------------------------
C.
C.    MATRIX ELEMENT FOR OMEGA AND PHI DECAYS              (MATRIX = 2)
C.    LAST UPDATE: 10/04/88
C.
C.----------------------------------------------------------------------
#include "eurodec/result.inc"
      NTP1=NTEIL+1
      NTP2=NTEIL+2
      NTP3=NTEIL+3
      DOT12=DOT(NTP1,NTP2)
      DOT13=DOT(NTP1,NTP3)
      DOT23=DOT(NTP2,NTP3)
      OMEPHI=(PTEIL(5,NTP1)*PTEIL(5,NTP2)*PTEIL(5,NTP3))**2-(PTEIL
     &(5,NTP1)*DOT23)** 2-(PTEIL(5,NTP2)*DOT13)**2-(PTEIL(5,NTP3)
     &*DOT12)** 2+2.*DOT12*DOT23*DOT13
      OMEPHI=160.*OMEPHI/(PTEIL(5,IP)**6)
      RETURN
      END
