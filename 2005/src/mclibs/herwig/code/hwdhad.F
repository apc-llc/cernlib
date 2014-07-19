CDECK  ID>, HWDHAD.
*CMZ :-        -26/04/91  14.01.26  by  Federico Carminati
*-- Author :    Ian Knowles, Bryan Webber & Mike Seymour
C-----------------------------------------------------------------------
      SUBROUTINE HWDHAD
C-----------------------------------------------------------------------
C     GENERATES DECAYS OF UNSTABLE HADRONS AND LEPTONS
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWRGEN,HWULDO,RN,BF,COSANG,RSUM,DIST(4),VERTX(4),
     & PMIX,WTMX,WTMX2,XS,DOT1,DOT2
      INTEGER IHEP,ID,MHEP,IDM,I,IDS,IM,MO,IPDG
      LOGICAL STABLE
      EXTERNAL HWRGEN,HWDPWT,HWDWWT,HWULDO
      IF (IERROR.NE.0) RETURN
      DO 100 IHEP=1,NMXHEP
      IF (IHEP.GT.NHEP) THEN
        ISTAT=90
        RETURN
      ELSEIF (ISTHEP(IHEP).EQ.120 .AND.
     &  JDAHEP(1,IHEP).EQ.IHEP.AND.JDAHEP(2,IHEP).EQ.IHEP) THEN
C---COPY COLOUR SINGLET CMF
        NHEP=NHEP+1
        IF (NHEP.GT.NMXHEP) CALL HWWARN('HWDHAD',100,*999)
        CALL HWVEQU(5,PHEP(1,IHEP),PHEP(1,NHEP))
        CALL HWVEQU(4,VHEP(1,IHEP),VHEP(1,NHEP))
        IDHW(NHEP)=IDHW(IHEP)
        IDHEP(NHEP)=IDHEP(IHEP)
        ISTHEP(NHEP)=190
        JMOHEP(1,NHEP)=IHEP
        JMOHEP(2,NHEP)=NHEP
        JDAHEP(2,NHEP)=NHEP
        JDAHEP(1,IHEP)=NHEP
        JDAHEP(2,IHEP)=NHEP
      ELSEIF (ISTHEP(IHEP).GE.190.AND.ISTHEP(IHEP).LE.193) THEN
C---FIRST CHECK FOR STABILITY
        ID=IDHW(IHEP)
        IF (RSTAB(ID)) THEN
          ISTHEP(IHEP)=1
          JDAHEP(1,IHEP)=0
          JDAHEP(2,IHEP)=0
C---SPECIAL FOR GAUGE BOSON DECAY
          IF (ID.GE.198.AND.ID.LE.200) CALL HWDBOS(IHEP)
C---SPECIAL FOR HIGGS BOSON DECAY
          IF (ID.EQ.201) CALL HWDHIG(ZERO)
        ELSE
C---UNSTABLE.
C Calculate position of decay vertex
          IF (DKLTM(ID).EQ.0.) THEN
            CALL HWVEQU(4,VHEP(1,IHEP),VERTX)
            MHEP=IHEP
            IDM=ID
          ELSE
            CALL HWUDKL(ID,PHEP(1,IHEP),DIST)
            CALL HWVSUM(4,VHEP(1,IHEP),DIST,VERTX)
            IF (MAXDKL) THEN
              CALL HWDXLM(VERTX,STABLE)
              IF (STABLE) THEN
                ISTHEP(IHEP)=1
                JDAHEP(1,IHEP)=0
                JDAHEP(2,IHEP)=0
                GOTO 100
              ENDIF
            ENDIF
            IF (MIXING.AND.(ID.EQ.221.OR.ID.EQ.223.OR.
     &                      ID.EQ.245.OR.ID.EQ.247)) THEN
C Select flavour of decaying b-meson allowing for flavour oscillation
              IDS=MOD(ID,3)
              PMIX=HALF*(ONE+SIN(XMRCT(IDS)*VERTX(4)/PHEP(4,IHEP))/
     &                      COSH(YMRCT(IDS)*VERTX(4)/PHEP(4,IHEP)))
              IF (HWRGEN(1).LE.PMIX) THEN
                IF (ID.LE.223) THEN
                  IDM=ID+24
                ELSE
                  IDM=ID-24
                ENDIF
              ELSE
                IDM=ID
              ENDIF
C Introduce a decaying neutral b-meson
              IF (NHEP+1.GT.NMXHEP) CALL HWWARN('HWDHAD',101,*999)
              MHEP=NHEP+1
              ISTHEP(MHEP)=ISTHEP(IHEP)
              ISTHEP(IHEP)=200
              JDAHEP(1,IHEP)=MHEP
              JDAHEP(2,IHEP)=MHEP
              IDHW(MHEP)=IDM
              IDHEP(MHEP)=IDPDG(IDM)
              JMOHEP(1,MHEP)=IHEP
              JMOHEP(2,MHEP)=JMOHEP(2,IHEP)
              CALL HWVEQU(5,PHEP(1,IHEP),PHEP(1,MHEP))
              CALL HWVEQU(4,VERTX,VHEP(1,MHEP))
              NHEP=NHEP+1
            ELSE
              MHEP=IHEP
              IDM=ID
            ENDIF
          ENDIF
C Use CLEO/EURODEC packages for b-hadrons if requested
          IF ((IDM.GE.221.AND.IDM.LE.231).OR.
     &        (IDM.GE.245.AND.IDM.LE.254)) THEN
            IF (BDECAY.EQ.'CLEO') THEN
              CALL HWDCLE(MHEP)
              GOTO 100
            ELSEIF (BDECAY.EQ.'EURO') THEN
              CALL HWDEUR(MHEP)
              GOTO 100
            ENDIF
          ENDIF
C Choose decay mode
          ISTHEP(MHEP)=ISTHEP(MHEP)+5
          RN=HWRGEN(2)
          BF=0.
          IM=LSTRT(IDM)
          DO 10 I=1,NMODES(IDM)
          BF=BF+BRFRAC(IM)
          IF (BF.GE.RN) GOTO 20
  10      IM=LNEXT(IM)
          CALL HWWARN('HWDHAD',50,*20)
  20      IF ((IDKPRD(1,IM).GE.1.AND.IDKPRD(1,IM).LE.13).OR.
     &        (IDKPRD(3,IM).GE.1.AND.IDKPRD(3,IM).LE.13)) THEN
C Partonic decay of a heavy-(b,c)-hadron, store details
            NQDK=NQDK+1
            IF (NQDK.GT.NMXQDK) CALL HWWARN('HWDHAD',102,*999)
            LOCQ(NQDK)=MHEP
            IMQDK(NQDK)=IM
            CALL HWVEQU(4,VERTX,VTXQDK(1,NQDK))
            GOTO 100
          ELSE
C Exclusive decay, add decay products to event record
            IF (NHEP+NPRODS(IM).GT.NMXHEP)
     &        CALL HWWARN('HWDHAD',103,*999)
            JDAHEP(1,MHEP)=NHEP+1
            DO 30 I=1,NPRODS(IM)
            NHEP=NHEP+1
            IDHW(NHEP)=IDKPRD(I,IM)
            IDHEP(NHEP)=IDPDG(IDKPRD(I,IM))
            ISTHEP(NHEP)=193
            JMOHEP(1,NHEP)=MHEP
            JMOHEP(2,NHEP)=JMOHEP(2,MHEP)
            PHEP(5,NHEP)=RMASS(IDKPRD(I,IM))
  30        CALL HWVEQU(4,VERTX,VHEP(1,NHEP))
            JDAHEP(2,MHEP)=NHEP
          ENDIF
C Next choose momenta:
          IF (NPRODS(IM).EQ.1) THEN
C 1-body decay: K0(BR) --> K0S,K0L
            CALL HWVEQU(4,PHEP(1,MHEP),PHEP(1,NHEP))
          ELSEIF (NPRODS(IM).EQ.2) THEN
C 2-body decay
C---SPECIAL TREATMENT OF POLARIZED MESONS
            COSANG=TWO
            IF (ID.EQ.IDHW(JMOHEP(1,MHEP))) THEN
              MO=JMOHEP(1,MHEP)
              RSUM=0
              DO 40 I=1,3
  40          RSUM=RSUM+RHOHEP(I,MO)
              IF (RSUM.GT.0) THEN
                RSUM=RSUM*HWRGEN(3)
                IF (RSUM.LT.RHOHEP(1,MO)) THEN
C---(1+COSANG)**2
                  COSANG=MAX(HWRGEN(4),HWRGEN(5),HWRGEN(6))*TWO-ONE
                ELSEIF (RSUM.LT.RHOHEP(1,MO)+RHOHEP(2,MO)) THEN
C---1-COSANG**2
                  COSANG=2*COS((ACOS(HWRGEN(7)*TWO-ONE)+PIFAC)/THREE)
                ELSE
C---(1-COSANG)**2
                  COSANG=MIN(HWRGEN(8),HWRGEN(9),HWRGEN(10))*TWO-ONE
                ENDIF
              ENDIF
            ENDIF
            CALL HWDTWO(PHEP(1,MHEP),PHEP(1,NHEP-1),
     &                  PHEP(1,NHEP),CMMOM(IM),COSANG,.FALSE.)
          ELSEIF (NPRODS(IM).EQ.3) THEN
C 3-body decay
            IF (NME(IM).EQ.100) THEN
C  Use free massless (V-A)*(V-A) Matrix Element
              CALL HWDTHR(PHEP(1,MHEP),PHEP(1,NHEP-1),PHEP(1,NHEP-2),
     &                    PHEP(1,NHEP),HWDWWT)
            ELSEIF (NME(IM).EQ.101) THEN
C  Use bound massless (V-A)*(V-A) Matrix Element
              WTMX=((PHEP(5,MHEP)-PHEP(5,NHEP))
     &             *(PHEP(5,MHEP)+PHEP(5,NHEP))
     &             +(PHEP(5,NHEP-1)-PHEP(5,NHEP-2))
     &             *(PHEP(5,NHEP-1)+PHEP(5,NHEP-2)))/TWO
              WTMX2=WTMX**2
              IPDG=ABS(IDHEP(MHEP))
              XS=ONE-MAX(RMASS(MOD(IPDG/1000,10)),
     &                   RMASS(MOD(IPDG/100,10)),RMASS(MOD(IPDG/10,10)))
     &              /(RMASS(MOD(IPDG/1000,10))+RMASS(MOD(IPDG/100,10))
     &               +RMASS(MOD(IPDG/10,10)))
  50          CALL HWDTHR(PHEP(1,MHEP),PHEP(1,NHEP-1),PHEP(1,NHEP-2),
     &                    PHEP(1,NHEP),HWDWWT)
              DOT1=HWULDO(PHEP(1,MHEP),PHEP(1,NHEP-1))
              DOT2=HWULDO(PHEP(1,MHEP),PHEP(1,NHEP-2))
              IF (DOT1*(WTMX-DOT1-XS*DOT2).LT.HWRGEN(11)*WTMX2) GOTO 50
            ELSE
              CALL HWDTHR(PHEP(1,MHEP),PHEP(1,NHEP-2),PHEP(1,NHEP-1),
     &                    PHEP(1,NHEP),HWDPWT)
            ENDIF
          ELSEIF (NPRODS(IM).EQ.4) THEN
C 4-body decay
            CALL HWDFOR(PHEP(1,MHEP  ),PHEP(1,NHEP-3),PHEP(1,NHEP-2),
     &                  PHEP(1,NHEP-1),PHEP(1,NHEP))
          ELSEIF (NPRODS(IM).EQ.5) THEN
C 5-body decay
            CALL HWDFIV(PHEP(1,MHEP  ),PHEP(1,NHEP-4),PHEP(1,NHEP-3),
     &                  PHEP(1,NHEP-2),PHEP(1,NHEP-1),PHEP(1,NHEP))
          ELSE
            CALL HWWARN('HWDHAD',104,*999)
          ENDIF
        ENDIF
      ENDIF
  100 CONTINUE
C---MAY HAVE OVERFLOWED /HEPEVT/
      CALL HWWARN('HWDHAD',105,*999)
  999 END
