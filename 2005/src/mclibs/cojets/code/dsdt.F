*
* $Id: dsdt.F,v 1.1.1.1 1996/01/11 14:14:35 mclareni Exp $
*
* $Log: dsdt.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:35  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE DSDT(IM,S,T,U,WTDSDT,WFDSDT)
C     ***************************************
C-- DSIGMA/DT OF HARD PROCESSES
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
#include "cojets/mflain.inc"
#include "cojets/nflav.inc"
#include "cojets/qcds.inc"
#include "cojets/thrfla.inc"
      DIMENSION WFDSDT(6)
C
      DO 1 L=1,6
1     WFDSDT(L)=0.
      WTDSDT=0.
      S2=S**2
      T2=T**2
      U2=U**2
      IF(IM.GT.5) GO TO 300
      IF(KPRHEV.EQ.1) GO TO 100
      IF(IM.EQ.1) GO TO 10
      IF(IM.EQ.2) GO TO 20
      IF(IM.EQ.3) GO TO 30
      IF(IM.EQ.4) GO TO 40
      IF(IM.EQ.5) GO TO 50
C
C-- Q-Q --+ Q-Q NON-ID.
10    WFDSDT(1)=(4./9.)*(S2+U2)/T2
      WTDSDT=WFDSDT(1)
      RETURN
C
C-- Q-Q --+ Q-Q ID.
20    WFDSDT(1)=(4./9.)*((S2+U2)/T2+(S2+T2)/U2)-(8./27.)*S2/(U*T)
      WFDSDT(1)=.5*WFDSDT(1)
      WTDSDT=WFDSDT(1)
      RETURN
C
C-- QB-Q --+ QB-Q ANNIH. F=I
30    WFDSDT(1)=(4./9.)*((S2+U2)/T2+(T2+U2)/S2)-(8./27.)*U2/(S*T)
C-- QB-Q --+ QB-Q ANNIH. F.NE.I
      WFDSDT(2)=(4./9.)*(T2+U2)/S2*2.
C-- QB-Q --+ G-G
      WFDSDT(3)=(32./27.)*(U2+T2)/(U*T)-(8./3.)*(U2+T2)/S2
      WFDSDT(3)=.5*WFDSDT(3)
C-- QB-Q --+ HEAVY QUARKS
      LF=3
      DO 31 L=4,6
      IF(NFLAV.LT.L) GO TO 35
      IF(S.LE.THRFLA(L)) GO TO 35
      LF=L
      WFDSDT(L)=DSHVQ(AM2HEV(L),S,T,U)
31    CONTINUE
35    CONTINUE
      WTDSDT=0.
      DO 36 L=1,LF
36    WTDSDT=WTDSDT+WFDSDT(L)
      RETURN
C
C-- Q-G --+ Q-G
40    WFDSDT(1)=-(4./9.)*(U2+S2)/(U*S)+(U2+S2)/T2
      WTDSDT=WFDSDT(1)
      RETURN
C
C-- G-G --+ G-G
50    WFDSDT(1)=(9./2.)*(3.-U*T/S2-U*S/T2-S*T/U2)
      WFDSDT(1)=.5*WFDSDT(1)
C-- G-G --+ Q-QB
      WFDSDT(2)=(1./6.)*(U2+T2)/(U*T)-(3./8.)*(U2+T2)/S2
      WFDSDT(2)=WFDSDT(2)*3.
C-- G-G --+ HEAVY QUARKS
      LF=2
      DO 51 L=4,6
      IF(NFLAV.LT.L) GO TO 55
      IF(S.LE.THRFLA(L)) GO TO 55
      LF=L
      WFDSDT(L)=DSHVG(AM2HEV(L),S,T,U)
51    CONTINUE
55    CONTINUE
      WTDSDT=0.
      DO 56 L=1,LF
56    WTDSDT=WTDSDT+WFDSDT(L)
      RETURN
C
C-- FORCING PRODUCTION OF HEAVY FLAVOR KFRFLA (FUSION PART)
100   IF(S.LE.THRFLA(KFRFLA)) GO TO 200
      IF(IM.EQ.1) GO TO 200
      IF(IM.EQ.2) GO TO 200
      IF(IM.EQ.3) GO TO 130
      IF(IM.EQ.4) GO TO 200
      IF(IM.EQ.5) GO TO 150
130   WFDSDT(KFRFLA)=DSHVQ(AM2HEV(KFRFLA),S,T,U)
      WTDSDT=WFDSDT(KFRFLA)
      RETURN
150   WFDSDT(KFRFLA)=DSHVG(AM2HEV(KFRFLA),S,T,U)
      WTDSDT=WFDSDT(KFRFLA)
      RETURN
200   RETURN
C
C-- FLAVOR EXCITATION SECTOR
300   CONTINUE
      MFLAX=0
      IF(MFLA1.NE.LGLUS) MFLAX=MFLA1
      IF(MFLA2.NE.LGLUS) MFLAX=MAX(MFLAX,MFLA2)
      IF(IM.EQ.6) GO TO 360
      IF(IM.EQ.7) GO TO 370
      A2=AM2HEV(MFLA1)
      B2=AM2HEV(MFLA2)
      IF(IM.EQ.8) GO TO 380
      IF(IM.EQ.9) GO TO 390
      IF(IM.EQ.10) GO TO 400
C
C-- Q-QH --+ Q-QH
360   IF(S.LE.AM2HEV(MFLAX)) RETURN
      WFDSDT(1)=DSEXQ(AM2HEV(MFLAX),S,T,U)
      WTDSDT=WFDSDT(1)
      RETURN
C
C-- G-QH --+ G-QH
370   IF(S.LE.AM2HEV(MFLAX)) RETURN
      WFDSDT(1)=DSEXG(AM2HEV(MFLAX),S,T,U)
      WTDSDT=WFDSDT(1)
      RETURN
C
C
C-- QH-QH --+ QH-QH NON-ID.
380   WFDSDT(1)=DQHNID(A2,B2,S,T,U)
      WTDSDT=WFDSDT(1)
      RETURN
C
C-- QH-QH --+ QH-QH ID.
390   WFDSDT(1)=DQHID(A2,S,T,U)
      WFDSDT(1)=.5*WFDSDT(1)
      WTDSDT=WFDSDT(1)
      RETURN
C
C-- QHB-QH --+ ANNIHILATION
400   CONTINUE
C-- QHB-QH --+ QHB-Q  F=I
      WFDSDT(1)=DQHFSI(A2,S,T,U)
C-- QHB-QH --+ QB-Q F=LIGHT
      WFDSDT(2)=(4./9.)*(T2+U2)/S2 *3.
C-- QHB-QH --+ G-G
      WFDSDT(3)=(32./27.)*(U2+T2)/(U*T)
     1         -(8./3.)*(U2+T2)/S2
      WFDSDT(3)=.5*WFDSDT(3)
C-- QHB-QH --+ HEAVY QUARKS F.NE.I
      DO 431 L=4,6
      IF(NFLAV.LT.L) GO TO 435
      IF(S.LE.THRFLA(L)) GO TO 435
      IF(L.EQ.MFLA1) GO TO 431
      WFDSDT(L)=DSHVQ(AM2HEV(L),S,T,U)
431   CONTINUE
435   CONTINUE
      WTDSDT=0.
      DO 436 L=1,6
436   WTDSDT=WTDSDT+WFDSDT(L)
      RETURN
      END
