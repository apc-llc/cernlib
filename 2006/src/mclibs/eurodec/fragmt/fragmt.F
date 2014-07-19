*
* $Id: fragmt.F,v 1.1.1.1 1996/03/08 16:58:51 mclareni Exp $
*
* $Log: fragmt.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:51  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE FRAGMT(IP1,IP2,NTEIL0)
C.----------------------------------------------------------------------
C.
C.    TREAT PARTONS IP1 TO IP2 IN COMMON /MOMGEN/:
C.    - FORWARD PARTON CASCADE,
C.    - HADRONIZATION: FRAGMENTATION F&F* AND RECOMBINATION,
C.    - ENERGY MOMENTUM CONSERVATION IN THE FRAME GIVEN BY IP1 - IP2,
C.    - ICASC = 1, INCLUDE PARTON CASCADE (SHOWER) IN FINAL STATE.
C.    LAST UPDATE: 10/04/89
C.
C.----------------------------------------------------------------------
#include "eurodec/eudopt.inc"
#include "eurodec/momgen.inc"
#include "eurodec/hadgen.inc"
#include "eurodec/result.inc"
#include "eurodec/ptable.inc"
#include "eurodec/convrt.inc"
#include "eurodec/inivtx.inc"
      DIMENSION PRIVER(3),PBOOST(4),BET(3)
C--
C--   INITIALIZE AND KEEP START POINTERS
      NCUT=0
      NTEIS0=NTEIL0
      NTEILS=NTEIL
C--
C--   OPTIONAL SIMULATION OF PRIMARY VERTEX
      IF (IPVTX.EQ.1) THEN
        CALL DETVTX(PRIVER)
        DO 10 I=IP1,IP2
          DO 10 J=1,3
   10   PRIVTX(J,I)=PRIVER(J)
      ENDIF
   20 NPART=0
C--
C--   COPY PARTON / PARTICLE INFORMATION FROM /MOMGEN/
      DO 80 I=IP1,IP2
        ID=IABS(IP(I))
        IF (ID.LT.90) THEN
C--
C--   COPY PARTON INFORMATION INTO LOCAL COMMON /HADGEN/
          NPART=NPART+1
          IF (NPART.GT.NHMAX) CALL ERRORD(62,' ',FLOAT(NHMAX))
          IH(NPART)=IP(I)
          IPP(NPART)=-I
          AHM(NPART)=APM(I)
          IHH(NPART)=IPHEL(I)
          DO 30 J=1,5
   30     PHA(J,NPART)=PCM(J,I)
        ELSE
C--
C--   COPY PARTICLES TO /RESULT/
          NTEIL=NTEIL+1
          IF (NTEIL.GT.NTMAX) CALL ERRORD(61,' ',FLOAT(NTMAX))
          INDEXT(NTEIL)=IP(I)
          IORIGT(NTEIL)=-(I*10000+I)
          IDCAYT(NTEIL)=0
          ITHEL(NTEIL)=IPHEL(I)
          DO 40 J=1,4
   40     PTEIL(J,NTEIL)=PCM(J,I)
          PTEIL(5,NTEIL)=APM(I)
C--
C--   TREAT DECAYS OF LEPTOQUARKS ETC. BEFORE FRAGMENTATION...
          IF ((ID.GT.1000).AND.(ID.LT.1010)) THEN
            NT=NTEIL
            NTM1=NTEIL-1
            IPPAR=ICONV(ID)
            IPDEC=IDP(IPPAR)
            CALL PARDEC(NT,IPPAR,IPDEC)
C--
C--   REPLACE PARENT IN RESULT BY DECAY PRODUCTS
            NTP1=NT+1
            NTPNH=NTEIL
            NTEIL=NTM1
            DO 70 J=NTP1,NTPNH
              NTEIL=NTEIL+1
              INDEXT(NTEIL)=INDEXT(J)
              IORIGT(NTEIL)=-(I*10000+I)
              IDCAYT(NTEIL)=0
              ITHEL(NTEIL)=ITHEL(J)
              DO 50 K=1,5
   50         PTEIL(K,NTEIL)=PTEIL(K,J)
              IF (IABS(INDEXT(J)).LT.90) THEN
                NPART=NPART+1
                IF (NPART.GT.NHMAX) CALL ERRORD(62,' ',
     &          FLOAT(NHMAX))
                IH(NPART)=INDEXT(J)
                IPP(NPART)=NTEIL
                AHM(NPART)=PTEIL(5,J)
                IHH(NPART)=ITHEL(J)
                DO 60 K=1,4
   60           PHA(K,NPART)=PTEIL(K,J)
                PHA(5,NPART)=SQRT((PHA(4,NPART)-AHM(NPART))*
     &          (PHA(4,NPART)+AHM(NPART)))
              ENDIF
   70       CONTINUE
            NTEIL0=NTEIL
          ENDIF
        ENDIF
   80 CONTINUE
      IF (NPART.GT.0) THEN
        SUMM=0.
        DO 90 I=1,4
   90   PBOOST(I)=0.
C--
C--   DETERMINE BOOST VECTOR AND SUM MASSES...
        DO 100 I=1,NPART
          SUMM=SUMM+AHM(I)
          DO 100 J=1,4
  100   PBOOST(J)=PBOOST(J)-PHA(J,I)
C--
C--   ...INCLUDING ENERGY MOMENTUM OF UNDERLYING EVENT
        IF (NTEIS0.LT.NTEILS) THEN
          DO 110 I=NTEIS0+1,NTEILS
            SUMM=SUMM+PTEIL(5,I)
            DO 110 J=1,4
  110     PBOOST(J)=PBOOST(J)-PTEIL(J,I)
        ENDIF
        PBOOST(4)=-PBOOST(4)
C--
C--   CHECK ON ENERGY-MOMENTUM DIFFERENCE
        Q0=PBOOST(4)**2-PBOOST(1)**2-PBOOST(2)**2-
     &  PBOOST(3)**2
        IF (Q0.LE.SUMM**2) CALL ERRORD(44,'FRAGMT',Q0)
        Q0=SQRT(Q0)
C--
C--   BOOST PARTONS TO THEIR CMS FRAME...
        IF (ABS(Q0-PBOOST(4)).GT.1.E-10) THEN
          DO 120 J=1,3
  120     BET(J)=PBOOST(J)/Q0
          GAM=PBOOST(4)/Q0
          DO 140 I=1,NPART
            PB=BET(1)*PHA(1,I)+BET(2)*PHA(2,I)+BET(3)*PHA(3,I)
            DO 130 J=1,3
  130       PHA(J,I)=PHA(J,I)+BET(J)*(PHA(4,I)+PB/(GAM+1.))
            PHA(4,I)=GAM*PHA(4,I)+PB
  140     PHA(5,I)=SQRT((PHA(4,I)-AHM(I))*(PHA(4,I)+AHM(I)))
C--
C--   ...AND PARTICLES FROM UNDERLYING EVENT
          IF (NTEIS0.LT.NTEILS) THEN
            DO 160 I=NTEIS0+1,NTEIL
              PB=BET(1)*PTEIL(1,I)+BET(2)*PTEIL(2,I)+BET(3)*PTEIL(3,I)
              DO 150 J=1,3
  150         PTEIL(J,I)=PTEIL(J,I)+BET(J)*(PTEIL(4,I)+PB/(GAM+1.))
  160       PTEIL(4,I)=GAM*PTEIL(4,I)+PB
          ENDIF
        ENDIF
C--
C--   FORWARD PARTON SHOWER
        IF (ICASC.EQ.1) CALL FINSHO(1,NPART)
C--
C--   SPLIT GLUONS: G ---> Q QBAR
        NSTART=NPART
        DO 170 I=1,NSTART
          IF (IABS(IH(I)).EQ.9) THEN
            NTP1=NTEIL+1
            CALL GJETFR(I,NPART,NTEIL)
C--
C--   GLUON SPLITTING HISTORY
            IOR=10000*IPP(I)+IPP(I)
            IPP(I)=NTP1
            IORIGT(NTP1)=IOR
            IDCAYT(NTP1)=0
            IPP(NPART)=NTEIL
            IORIGT(NTEIL)=IOR
            IDCAYT(NTEIL)=0
          ENDIF
  170   CONTINUE
C--
C--   FRAGMENT RESULTING PARTONS (QUARKS/DIQUARKS)
        DO 190 I=1,NPART
          NTP1=NTEIL+1
          CALL QJETFR(I,NTEIL)
          NTEIL=NTEIL+1
          IF (IPP(I).GT.0) THEN
            IOR=10000*(ABS(IORIGT(IPP(I)))/10000)+IPP(I)
            IPP(I)=IPP(I)*10000+NTEIL
          ELSE
            IOR=IPP(I)*10000+IPP(I)
            IPP(I)=IPP(I)*10000-NTEIL
          ENDIF
          DO 180 J=NTP1,NTEIL
            IORIGT(J)=IOR
  180     IDCAYT(J)=0
  190   IF (IPP(I).GT.0) IDCAYT(IPP(I)/10000)=NTP1*10000+NTEIL
C--
C--   COLLECT 'SOFT' PARTONS AND HADRONIZE
        CALL CLRSTK(1,NPART)
C--
C--   CONSERVE ENERGY AND MOMENTUM
        CALL ADJUST(Q0,NTEIL0+1,NTEIL,IADCUT)
        IF (IADCUT.EQ.1) THEN
          NCUT=NCUT+1
          IF (NCUT.GE.100) CALL ERRORD(43,' ',FLOAT(NCUT))
          NTEIL=NTEILS
          NTEIL0=NTEIS0
          GOTO 20
        ENDIF
C--
C--   BOOST PARTONS/PARTICLES BACK TO THEIR LAB FRAME
        IF (ABS(Q0-PBOOST(4)).GT.1.E-10) THEN
          DO 210 I=NTEIL0+1,NTEIL
            PB=-BET(1)*PTEIL(1,I)-BET(2)*PTEIL(2,I)-BET(3)*PTEIL(3,I)
            DO 200 J=1,3
  200       PTEIL(J,I)=PTEIL(J,I)-BET(J)*(PTEIL(4,I)+PB/(GAM+1.))
  210     PTEIL(4,I)=GAM*PTEIL(4,I)+PB
        ENDIF
      ENDIF
C--
C--   FILL VERTEX INFORMATION FOR PARTICLES FROM FRAGMENTATION
      DO 220 I=NTEILS+1,NTEIL
        DO 220 J=1,3
  220 SECVTX(J,I)=PRIVTX(J,IP1)
C--
C--   LET ALL UNSTABLE PARTICLES DECAY...
      CALL DECAYS(MIN(NTEIL0,NTEILS)+1,NTEIL)
      RETURN
      END
