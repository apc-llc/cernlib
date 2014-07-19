*
* $Id: jets.F,v 1.1.1.1 1996/03/08 16:58:53 mclareni Exp $
*
* $Log: jets.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:53  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE JETS(IP,IP1,IP2,NJETS,IADCUT)
C.----------------------------------------------------------------------
C.
C.    TREAT PARTONS/PARTICLES IP1 TO IP2 IN COMMON /RESULT/:
C.    - FORWARD PARTON CASCADE,
C.    - HADRONIZATION: F&F* FRAGMENTATION AND RECOMBINATION,
C.    - ENERGY MOMENTUM CONSERVATION,
C.    - IADCUT = 1, REJECT EVENT.
C.    LAST UPDATE: 15/04/89
C.
C.----------------------------------------------------------------------
#include "eurodec/eudopt.inc"
#include "eurodec/hadgen.inc"
#include "eurodec/result.inc"
#include "eurodec/ptable.inc"
#include "eurodec/convrt.inc"
      DIMENSION PSPECT(3)
      DATA SIGMA, TRUNC/ 0.15, 0.02/
      IADCUT=0
      NTPNT=NTEIL+IP2-IP1+1
      NPART=0
C--
C--   ISOLATE 'SINGLE QUARK' ONIUM DECAYS: IDIR = 0!
      ID1=ABS(INDEXT(IP))/100
      ID2=(ABS(INDEXT(IP))-ID1*100)/10
      IDIR=1
      IDABS=ABS(INDEXT(IP1))
      IF ((ID1.EQ.ID2).AND.(ID1.GT.5)) THEN
C--
C--   EXCLUDE HEAVY ONIUM CASCADE DECAYS
        ID3=IDABS/100
        ID4=(IDABS-ID3*100)/10
        IF ((ID3.EQ.ID4).AND.(ID3.GT.0)) THEN
          IADCUT=0
          RETURN
        ENDIF
        IF ((IP2-IP1).GE.3) IDIR=0
      ENDIF
      IF ((NJETS.EQ.(IP2-IP1+1)).AND.(IDIR.EQ.1)) THEN
C--
C--   COPY PARTON INFORMATION TO /HADGEN/...
        DO 20 I=IP1,IP2
          NPART=NPART+1
          IF (NPART.GT.NHMAX) CALL ERRORD(62,' ',FLOAT(NHMAX))
          IPP(NPART)=I
          IH(NPART)=INDEXT(I)
          AHM(NPART)=PTEIL(5,I)
          IHH(NPART)=ITHEL(I)
          DO 10 J=1,4
   10     PHA(J,NPART)=PTEIL(J,I)
   20   PHA(5,NPART)=SQRT((PHA(4,NPART)-AHM(NPART))*(PHA(4,NPART)+AHM
     &  (NPART)))
C--
C--   ...AND FRAGMENT ALL PARTONS IN FRAME GIVEN BY Q0
        Q0=PTEIL(5,IP)
        IADCUT=1
        CALL PARFRA(Q0,NPART,NTPNT,IADCUT)
        IP2=NTPNT
      ELSE
C--
C--   FRAGMENT HEAVY FLAVOUR QUARK-SPECTATOR SYSTEM?
        IOFF=0
        IF (IHVYFR.EQ.1) THEN
C--
C--   DERIVE CONSTITUENT PARTON CODES FROM BARYON/MESON
          IF (IDABS.GE.5000) THEN
            NPART=NPART+2
            IH(1)=ISIGN(IDABS/1000,INDEXT(IP1))
            IH(2)=ISIGN(IDABS/10-100*ABS(IH(1)),INDEXT(IP1))
          ELSEIF ((IDABS.GE.500).AND.(IDABS.LT.1000)) THEN
            NPART=NPART+2
            IH(1)=ISIGN(IDABS/100,INDEXT(IP1))
            IH(2)=-ISIGN(IDABS/10-10*ABS(IH(1)),INDEXT(IP1))
          ENDIF
C--
C--   DETERMINE ENERGY/MOMENTA OF PARTONS...
          IF (NPART.EQ.2) THEN
            IOFF=1
C--
C--   REPLACE PARTICLE CODE BY PARTON CODE IN /RESULT/
            INDEXT(IP1)=IH(1)
            IPP(1)=IP1
            IPP(2)=IP1
   30       CALL FERMI(PSPECT,SIGMA,TRUNC)
            DO 40 J=1,3
              PHA(J,2)=PSPECT(J)
   40       PHA(J,1)=PTEIL(J,IP1)-PSPECT(J)
            DO 50 I=1,NPART
              AHM(I)=PM(ICONV(ABS(IH(I))))
              PHA(5,I)=PHA(1,I)**2+PHA(2,I)**2+PHA(3,I)**2
              PHA(4,I)=SQRT(PHA(5,I)+AHM(I)**2)
              PHA(5,I)=SQRT(PHA(5,I))
              IF (PHA(5,I).LT.0.02) GOTO 30
   50       CONTINUE
C--
C--   ...AND FRAGMENT
            Q0SUBS=-1.
            NTP1=NTPNT+1
            IADCUT=-1
            CALL PARFRA(Q0SUBS,NPART,NTPNT,IADCUT)
C--
C--   SPECIAL! MODIFY DECAY POINTER
            IDCAYT(IP1)=NTP1*10000+NTPNT
          ENDIF
        ENDIF
C--
C--   FRAGMENT OTHER PARTONS 'PAIR-WISE'
        I=IP1+IOFF-1
   60   NPART=0
   70   I=I+1
        ID=ABS(INDEXT(I))
        IF (ID.LT.90) THEN
C--
C--   COPY PARTON INFORMATION
          NPART=NPART+1
          IF (NPART.GT.NHMAX) CALL ERRORD(62,' ',FLOAT(NHMAX))
          IPP(NPART)=I
          IH(NPART)=INDEXT(I)
          AHM(NPART)=PTEIL(5,I)
          IHH(NPART)=ITHEL(I)
          DO 80 J=1,4
   80     PHA(J,NPART)=PTEIL(J,I)
          PHA(5,NPART)=SQRT((PHA(4,NPART)-AHM(NPART))*(PHA(4,NPART)+
     &    AHM(NPART)))
          IF (NPART.EQ.2) THEN
            Q0SUBS=-1.
            IF (IOFF.EQ.1) THEN
              IADCUT=-1
              CALL PARFRA(Q0SUBS,NPART,NTPNT,IADCUT)
            ELSE
              IADCUT=1
              CALL PARFRA(Q0SUBS,NPART,NTPNT,IADCUT)
              IF (IADCUT.EQ.1) RETURN
            ENDIF
            IF (I.LT.IP2) GOTO 60
          ENDIF
        ENDIF
        IF (I.LT.IP2) GOTO 70
        IP2=NTPNT
        IF (IOFF.EQ.1) THEN
C--
C--   IMPOSE ENERGY MOMENTUM CONSERVATION...
          Q0=PTEIL(5,IP)
          IADCUT=1
          CALL ADJUST(Q0,IP1,IP2,IADCUT)
        ENDIF
      ENDIF
      RETURN
      END
