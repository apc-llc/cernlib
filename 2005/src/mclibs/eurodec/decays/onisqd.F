*
* $Id: onisqd.F,v 1.1.1.1 1996/03/08 16:58:53 mclareni Exp $
*
* $Log: onisqd.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:53  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION ONISQD(IP,VIRTMA)
C.----------------------------------------------------------------------
C.
C.    MATRIX ELEMENT FOR WEAK (V - A) IN ONIUM SQ DECAYS  (MATRIX = 24)
C.    LAST UPDATE: 20/03/89
C.
C.----------------------------------------------------------------------
#include "eurodec/result.inc"
      DIMENSION PTOP(4)
C--
C--   BREIT-WIGNER ALREADY INCLUDED IN PHASE SPACE GENERATION...
      NTP2=NTEIL+2
      NTP3=NTEIL+3
      NTP4=NTEIL+4
C--
C--   CALCULATE VIRTUAL TOP QUARK ENERGY/MOMENTUM
      DO 10 J=1,4
   10 PTOP(J)=0.
      DO 20 I=NTP2,NTP4
        DO 20 J=1,4
   20 PTOP(J)=PTOP(J)+PTEIL(J,I)
      DOTT3=PTOP(4)*PTEIL(4,NTP3)-PTOP(1)*PTEIL(1,NTP3)-PTOP(2)*
     &PTEIL(2,NTP3)-PTOP(3)*PTEIL(3,NTP3)
      DOT24=DOT(NTP2,NTP4)
      ONISQD=16.*DOT24*DOTT3/(VIRTMA**4)
      RETURN
      END
