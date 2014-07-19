*
* $Id: tauvec.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: tauvec.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION TAUVEC(IP,INIT)
C.----------------------------------------------------------------------
C.
C.    MATRIX ELEMENT FOR POLARIZED TAU ---> TAUNEU + VECTOR,
C.    VECTOR = K*, RHO, A1                                 (MATRIX = 5)
C.    LAST UPDATE: 01/03/89
C.
C.----------------------------------------------------------------------
#include "eurodec/result.inc"
      SAVE IHEL,NTP2,AMV2,PTAU,PV
      DATA IHEL,NTP2,AMV2,PTAU,PV/ 0, 0, 0., 0., 0./
C--
C--   CALCULATE SOME QUANTITIES ONLY ONCE
      IF (INIT.EQ.0) THEN
        INIT=INIT+1
        NTP2=NTEIL+2
        AMV2=PTEIL(5,NTP2)**2
        PTAU=SQRT((PTEIL(4,IP)-PTEIL(5,IP))*(PTEIL(4,IP)+PTEIL(5,IP)))
        PV=SQRT((PTEIL(4,NTP2)-PTEIL(5,NTP2))*(PTEIL(4,NTP2)+PTEIL
     &  (5,NTP2)))
C--
C--   SELECT HELICITY STATE OF THE VECTOR PARTICLE (0 OR -1)
        RAT=2./(1.+2.*PTEIL(5,IP)*PV/AMV2)
        IHEL=0
        IF (EURRAN(IRDUM).LE.(RAT/(1.+RAT))) IHEL=-1
      ENDIF
      COST=(PTEIL(1,IP)*PTEIL(1,NTP2)+PTEIL(2,IP)*PTEIL(2,NTP2)+PTEIL
     &(3,IP)*PTEIL(3,NTP2))/(PTAU*PV)
C--
C--   CALCULATE AMPLITUDE FOR HELICITY STATE OF VECTOR (0 OR -1)
      IF (IHEL.EQ.0) THEN
        TAUVEC=(2.*PTEIL(5,IP)*(PV+FLOAT(ITHEL(IP))*PTEIL(4,NTP2)*COST)
     &  +AMV2*(1.-FLOAT(ITHEL(IP))*COST))/(2.*PTEIL(5,IP)*PV+AMV2)
      ELSE
        TAUVEC=1.-FLOAT(ITHEL(IP))*COST
        ITHEL(NTP2)=-1
      ENDIF
      TAUVEC=TAUVEC/2.
      RETURN
      END
