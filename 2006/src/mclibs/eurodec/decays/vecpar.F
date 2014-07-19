*
* $Id: vecpar.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: vecpar.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION VECPAR(IP,INIT)
C.----------------------------------------------------------------------
C.
C.    MATRIX ELEMENT FOR:
C.    SCALAR ---> VECTOR + SCALAR1 ---> SCALAR1 + SCALAR2  (MATRIX = 3)
C.    TAU ---> TAUNEUTRINO + VECTOR ---> TAUNEUTRINO + SCALAR1 + SCALAR2
C.    VECTOR = K*, RHO, ETC.
C.    LAST UPDATE: 01/03/89
C.
C.----------------------------------------------------------------------
#include "eurodec/result.inc"
      DIMENSION PPAR(3),BET(3)
      SAVE NTP1,PSCA,PVEC,PPAR
      DATA NTP1,PSCA,PVEC,PPAR/ 0, 0., 0., 0., 0., 0./
C--
C--   CALCULATE SOME QUANTITIES ONLY ONCE
      IF (INIT.EQ.0) THEN
        INIT=INIT+1
C--
C--   CHECK WHETHER PARENT OF DECAYING PARTICLE IS TAU OR PSEUDOSCALAR
        VECPAR=1.
        IF (IORIGT(IP).LE.0) RETURN
        JET=IORIGT(IP)/10000
        IOR=IORIGT(IP)-10000*JET
        IF (IOR.EQ.0) RETURN
        ID=ABS(INDEXT(IOR))
        IQ1=ID/100
        IQ2=(ID-IQ1*100)/10
        IF ((ID.EQ.95).OR.((ID.GT.100).AND.((ID-IQ1*100-IQ2*10).EQ.0)))
     &  THEN
C--
C--   DETERMINE MOMENTUM DECAYING VECTOR PARTICLE IN RESTFRAME PARENT
          DO 10 J=1,3
   10     BET(J)=-PTEIL(J,IOR)/PTEIL(5,IOR)
          GAM=PTEIL(4,IOR)/PTEIL(5,IOR)
          PB=BET(1)*PTEIL(1,IP)+BET(2)*PTEIL(2,IP)+BET(3)*PTEIL(3,IP)
C--
C--   BOOST TO REST FRAME...
          DO 20 J=1,3
   20     PPAR(J)=PTEIL(J,IP)+BET(J)*(PTEIL(4,IP)+PB/(GAM+1.))
          NTP1=NTEIL+1
          PVEC=SQRT(PPAR(1)**2+PPAR(2)**2+PPAR(3)**2)
          PSCA=SQRT((PTEIL(4,NTP1)-PTEIL(5,NTP1))*(PTEIL(4,NTP1)+PTEIL
     &    (5,NTP1)))
        ELSE
          RETURN
        ENDIF
      ENDIF
      COST=(PPAR(1)*PTEIL(1,NTP1)+PPAR(2)*PTEIL(2,NTP1)+PPAR(3)*
     &PTEIL(3,NTP1))/(PVEC*PSCA)
C--
C--   SELECT DISTRIBUTION ACCORDING TO HELICITY
      IF (ITHEL(IP).NE.0) THEN
        VECPAR=1.-COST**2
      ELSE
        VECPAR=COST**2
      ENDIF
      RETURN
      END
