CDECK  ID>, HWHEGW.
*CMZ :-        -26/04/91  10.18.56  by  Bryan Webber
*-- Author :    Mike Seymour
C-----------------------------------------------------------------------
      SUBROUTINE HWHEGW
C----------------------------------------------------------------------
C     W + GAMMA --> FF'BAR :  MEAN EVWGT = CROSS SECTION IN NANOBARN
C     BASED ON BOSON GLUON FUSION OF ABBIENDI AND STANCO
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWRGEN,GMASS,EV(3),RV,LEP,Y,Q2,SHAT,Z,PHI,AJACOB,
     & DSIGMA,ME,MP,ML,MREMIF(18),MFIN1(18),MFIN2(18),RS,SMA,W2,RSHAT
      INTEGER LEPFIN,ID1,ID2,I,IQK,IFLAVU,IFLAVD,IMIN,IMAX,IFL,IPROO
      LOGICAL CHARGD,INCLUD(18),INSIDE(18)
      EXTERNAL HWRGEN
      SAVE LEPFIN,ID1,ID2
      COMMON /HWAREA/ LEP,Y,Q2,SHAT,Z,PHI,AJACOB,DSIGMA,ME,MP,ML,MREMIF,
     & MFIN1,MFIN2,RS,SMA,W2,RSHAT,IQK,IFLAVU,IFLAVD,IMIN,IMAX,IFL,
     & IPROO,CHARGD,INCLUD,INSIDE
      IQK=MOD(IPROC,10)
      CHARGD=.TRUE.
      IF(GENEV) THEN
C
        IDHW(4)=IDHW(1)
        IDHW(5)=59
        IDHW(6)=15
        IDHW(7)=LEPFIN
        IDHW(8)=ID1
        IDHW(9)=ID2
        DO 1 I=4,9
    1   IDHEP(I)=IDPDG(IDHW(I))
C
        IFLAVD=ID1
        IFLAVU=ID2-6
C
        ISTHEP(4)=111
        ISTHEP(5)=112
        ISTHEP(6)=110
        ISTHEP(7)=113
        ISTHEP(8)=114
        ISTHEP(9)=114
C
        JMOHEP(1,4)=6
        JMOHEP(2,4)=7
        JMOHEP(1,5)=6
        JMOHEP(2,5)=5
        JMOHEP(1,6)=4
        JMOHEP(2,6)=5
        JMOHEP(1,7)=6
        JMOHEP(2,7)=4
        JMOHEP(1,8)=6
        JMOHEP(2,8)=9
        JMOHEP(1,9)=6
        JMOHEP(2,9)=8
        JDAHEP(1,4)=0
        JDAHEP(2,4)=7
        JDAHEP(1,5)=0
        JDAHEP(2,5)=5
        JDAHEP(1,6)=7
        JDAHEP(2,6)=9
        JDAHEP(1,7)=0
        JDAHEP(2,7)=4
        JDAHEP(1,8)=0
        JDAHEP(2,8)=9
        JDAHEP(1,9)=0
        JDAHEP(2,9)=8
C---COMPUTATION OF MOMENTA IN LABORATORY FRAME OF REFERENCE
C---Persuade HWHBKI that the gluon is actually a photon...
        GMASS=RMASS(13)
        RMASS(13)=0
        CALL HWHBKI
        RMASS(13)=GMASS
C---put the other outgoing lepton in as well
        IDHW(10)=IDHW(2)
        IDHEP(10)=IDPDG(IDHW(10))
        ISTHEP(10)=1
        JMOHEP(1,10)=2
        JMOHEP(2,10)=0
        JDAHEP(1,10)=0
        JDAHEP(2,10)=0
        JDAHEP(1,2)=5
        JDAHEP(2,2)=10
        CALL HWVDIF(4,PHEP(1,2),PHEP(1,5),PHEP(1,10))
        CALL HWUMAS(PHEP(1,10))
        NHEP=10
C
C---if antilepton was first, do charge conjugation
        IF (LEP.EQ.-1) THEN
          DO 27 I=7,9
            IF (IDHEP(I).NE.0 .AND. ABS(IDHEP(I)).LT.20) THEN
              IDHW(I)=IDHW(I) + 6*SIGN(1,IDHEP(I))
              IDHEP(I)=-IDHEP(I)
            ENDIF
 27       CONTINUE
        ENDIF
C
C---half the time, do charge conjugation and parity flip
        IF (HWRGEN(0).GT.0.5) THEN
          DO 2 I=4,10
            IF (IDHEP(I).NE.0 .AND. ABS(IDHEP(I)).LT.20) THEN
              IDHW(I)=IDHW(I) + 6*SIGN(1,IDHEP(I))
              IDHEP(I)=-IDHEP(I)
            ENDIF
            PHEP(1,I)=-PHEP(1,I)
            PHEP(2,I)=-PHEP(2,I)
            PHEP(3,I)=-PHEP(3,I)
 2        CONTINUE
          JMOHEP(1,10)=3-JMOHEP(1,10)
        ENDIF
C
      ELSE
C
        EVWGT=ZERO
C---LEP = 1 IF TRACK 1 IS A LEPTON, -1 FOR ANTILEPTON
        LEP=0.
        IF (IDHW(1).GE.121.AND.IDHW(1).LE.126) THEN
          LEP=1.
        ELSEIF (IDHW(1).GE.127.AND.IDHW(1).LE.132) THEN
          LEP=-1.
        ENDIF
        IF (LEP.EQ.0) CALL HWWARN('HWHEGW',500,*999)
C---program only works if beam and target are charge conjugates
        IF (LEP*(IDHW(2)-IDHW(1)).NE.6) CALL HWWARN('HWHEGW',501,*999)
C---program only works for equal energy beams colliding
        IF (PHEP(3,3).NE.0) CALL HWWARN('HWHEGW',503,*999)
C
C---FINAL STATE IS ALWAYS SET UP AS IF PARTICLE IS BEFORE ANTI-PARTICLE
C   AND THEN INVERTED IF NECESSARY
        LEPFIN = MIN(IDHW(1),IDHW(2))+1
        IF (IQK.LE.2) THEN
          IFLAVU=2
          IFLAVD=1
          ID1  = 1
          ID2  = 8
        ELSEIF (IQK.LE.4) THEN
          IFLAVU=4
          IFLAVD=3
          ID1  = 3
          ID2  =10
        ELSEIF (IQK.LE.6) THEN
          IFLAVU=6
          IFLAVD=5
          ID1  = 5
          ID2  =12
        ELSEIF (IQK.EQ.7) THEN
          IFLAVU=122
          IFLAVD=121
          ID1  = 121
          ID2  = 128
C---INTERFERENCE TERMS IN EE -> EE NUE NUEB  NEGLECTED: SIGMA UNRELIABLE
          IF (FSTWGT) CALL HWWARN('HWHEGW',1,*999)
        ELSEIF (IQK.EQ.8) THEN
          IFLAVU=124
          IFLAVD=123
          ID1  = 123
          ID2  = 130
        ELSEIF (IQK.EQ.9) THEN
          IFLAVU=126
          IFLAVD=125
          ID1  = 125
          ID2  = 132
        ELSE
          CALL HWWARN('HWHEGW',504,*999)
        ENDIF
        IF (IQK.GT.0) THEN
          IF (IQK.LE.6) IQK=0
          CALL HWHBRN(*999)
          CALL HWHEGX
          EVWGT = 2 * DSIGMA * AJACOB
          IF (EVWGT.LT.ZERO) EVWGT=ZERO
        ELSE
C---SUM OVER QUARK FLAVOURS
          CALL HWHBRN(*999)
          DO 3 I=1,3
            IF (SHAT.GT.(RMASS(IFLAVD)+RMASS(IFLAVU))**2) THEN
              CALL HWHEGX
              EV(I) = 2 * DSIGMA * AJACOB
              IF (EV(I).LT.ZERO) EV(I)=ZERO
            ELSE
              EV(I)=ZERO
            ENDIF
            EVWGT=EVWGT+EV(I)
            EV(I)=EVWGT
            IFLAVU=IFLAVU+2
            IFLAVD=IFLAVD+2
 3        CONTINUE
C---CHOOSE QUARK FLAVOUR
          RV=EV(3)*HWRGEN(1)
          IF (RV.LT.EV(1)) THEN
            ID1 = 1
            ID2 = 8
          ELSEIF (RV.LT.EV(2)) THEN
            ID1 = 3
            ID2 =10
          ELSE
            ID1 = 5
            ID2 =12
          ENDIF
        ENDIF
      ENDIF
  999 END
