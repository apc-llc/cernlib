*
* $Id: oni3g.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: oni3g.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION ONI3G(IP)
C.----------------------------------------------------------------------
C.
C.    MATRIX ELEMENT FOR ONIUM ---> G G G DECAYS          (MATRIX = 13)
C.    LAST UPDATE: 14/08/89
C.
C.----------------------------------------------------------------------
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION YGLU
#endif
#include "eurodec/result.inc"
#include "eurodec/glufix.inc"
      ONI3G=-1.
      NTP1=NTEIL+1
      NTP2=NTEIL+2
      NTP3=NTEIL+3
      IF ((PTEIL(4,NTP1).LT.EGLMIN).OR.(PTEIL(4,NTP2).LT.EGLMIN)
     &.OR.(PTEIL(4,NTP3).LT.EGLMIN)) RETURN
      X1=2.*PTEIL(4,NTP1)/PTEIL(5,IP)
      X2=2.*PTEIL(4,NTP2)/PTEIL(5,IP)
      X3=2.*PTEIL(4,NTP3)/PTEIL(5,IP)
      ONI3G=(((1.-X1)/(X2*X3))**2+((1.-X2)/(X1*X3))**2+
     &      ((1.-X3)/(X1*X2))**2)/2.
      RETURN
      END
