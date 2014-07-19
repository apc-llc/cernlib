*
* $Id: ongqbq.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: ongqbq.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION ONGQBQ(IP)
C.----------------------------------------------------------------------
C.
C.    MATRIX ELEMENT FOR ONIUM ---> G QBAR Q DECAYS       (MATRIX = 15)
C.    LAST UPDATE: 14/08/89
C.
C.----------------------------------------------------------------------
#include "eurodec/result.inc"
#include "eurodec/glufix.inc"
      ONGQBQ=-1.
      NTP1=NTEIL+1
      NTP2=NTEIL+2
      NTP3=NTEIL+3
      IF (PTEIL(4,NTP1).LT.EGLMIN) RETURN
C      X1=2.*PTEIL(4,NTP1)/PTEIL(5,IP)
C      X2=2.*PTEIL(4,NTP2)/PTEIL(5,IP)
C      X3=2.*PTEIL(4,NTP3)/PTEIL(5,IP)
C      ONGQBQ=(((1.-X1)/(X2*X3))**2+((1.-X2)/(X1*X3))**2+
C     &        ((1.-X3)/(X1*X2))**2)/2.
      ONGQBQ=2.
      RETURN
      END
