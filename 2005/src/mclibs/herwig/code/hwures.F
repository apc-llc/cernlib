CDECK  ID>, HWURES.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Ian Knowles & Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWURES
C-----------------------------------------------------------------------
C     Using properties of particle I supplied in HWUDAT checks particles
C     and antiparticles have compatible properties and sets   SWTEF(I) =
C     ( rep. enhancement factor)^2  - used in cluster decays
C     Finds iso-flavour hadrons and creates pointers for cluster decays.
C     Sets CLDKWT(K) =(2J+1) spin weight normalizing largest value to 1.
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      INTEGER NMXTMP
      PARAMETER (NMXTMP=20)
      DOUBLE PRECISION HWUPCM,HWUAEM,EPS,WTMX,REMMN,RWTMX,WTMP,
     & RESTMP(91),WTMX2,REMMN2,WT,CDWTMP(NMXTMP)
      INTEGER HWUANT,MAPF(89),MAPC(12,12),I,IANT,IABPDG,J,L,N,K,LTMP,
     & NCDKS,IMN,ITMP,LOCTMP(91),NTMP,NCDTMP(NMXTMP),IMN2
      EXTERNAL HWUANT,HWUPCM,HWUAEM
      PARAMETER (EPS=1.D-6)
      DATA MAPF/21,31,41,51,61,12,32,42,52,62,13,23,43,53,63,14,24,34,
     & 44,54,64,15,25,35,45,55,65,16,26,36,46,56,66,111,112,113,122,123,
     & 133,222,223,233,333,-111,-112,-113,-122,-123,-133,-222,-223,-233,
     & -333,114,124,134,224,234,334,-114,-124,-134,-224,-234,-334,115,
     & 125,135,225,235,335,-115,-125,-135,-225,-235,-335,116,126,136,
     & 226,236,336,-116,-126,-136,-226,-236,-336/
      DATA MAPC/90,1,2,47,45,44,48,46,49,3,4,5,6,90,7,50,47,45,51,48,52,
     & 8,9,10,11,12,91,51,48,46,52,49,53,13,14,15,37,40,41,6*0,57,69,81,
     & 35,37,38,6*0,55,67,79,34,35,36,6*0,54,66,78,38,41,42,6*0,58,70,
     & 82,36,38,39,6*0,56,68,80,39,42,43,6*0,59,71,83,16,17,18,63,61,60,
     & 64,62,65,19,20,21,22,23,24,75,73,72,76,74,77,25,26,27,28,29,30,
     & 87,85,84,88,86,89,31,32,33/
C Check particle/anti-particle properties are compatible
      WRITE(6,10)
  10  FORMAT(/10X,'Checking consistency of particle properties'/)
      DO 20 I=10,NRES
      IF (IDPDG(I).GT.0) THEN
        IANT=HWUANT(I)
        IF (IANT.EQ.20) GOTO 20
        IF (MOD(IDPDG(I)/1000,10).EQ.0.AND.
     &      MOD(IDPDG(I)/100 ,10).NE.0) THEN
          IF (MOD(IFLAV(I)/10-IFLAV(IANT),10).NE.0.OR.
     &        MOD(IFLAV(I)-IFLAV(IANT)/10,10).NE.0)
     &     WRITE(6,30) RNAME(I),IFLAV(I),IFLAV(IANT)
        ELSE
          IF (IFLAV(I)+IFLAV(IANT).NE.0)
     &     WRITE(6,30) RNAME(I),IFLAV(I),IFLAV(IANT)
        ENDIF
        IF (ICHRG(I)+ICHRG(IANT).NE.0)
     &   WRITE(6,40) RNAME(I),ICHRG(I),ICHRG(IANT)
        IF (ABS(RMASS(I)-RMASS(IANT)).GT.EPS)
     &   WRITE(6,50) RNAME(I),RMASS(I),RMASS(IANT)
        IF (ABS(RLTIM(I)-RLTIM(IANT)).GT.EPS)
     &   WRITE(6,60) RNAME(I),RLTIM(I),RLTIM(IANT)
        IF (ABS(RSPIN(I)-RSPIN(IANT)).GT.EPS)
     &   WRITE(6,70) RNAME(I),RSPIN(I),RSPIN(IANT)
      ENDIF
  20  CONTINUE
  30  FORMAT(10X,A8,' flavour code=',I4,5X,' antiparticle=',I4)
  40  FORMAT(10X,A8,' charge      =',I2,7X,' antiparticle=',I2)
  50  FORMAT(10X,A8,' mass        =',F7.3,2X,' antiparticle=',F7.3)
  60  FORMAT(10X,A8,' life time   =',E9.3,' antiparticle=',E9.3)
  70  FORMAT(10X,A8,' spin        =',F3.1,6X,' antiparticle=',F3.1)
C Compute resonance properties
      DO 80 I=21,NRES
C Compute representation weights for hadrons, used in cluster decays
      IABPDG=ABS(IDPDG(I))
      J=MOD(IABPDG,10)
      IF (J.EQ.2.AND.MOD(IABPDG/100,10).LT.MOD(IABPDG/10,10)) THEN
C Singlet (Lambda-like) baryon
        SWTEF(I)=SNGWT**2
      ELSEIF (J.EQ.4) THEN
C Decuplet baryon
        SWTEF(I)=DECWT**2
      ELSEIF(2*(J/2).NE.J) THEN
C Mesons: identify by spin, angular momentum & radial excitation
        J=(J-1)/2
        L= MOD(IABPDG/10000 ,10)
        N= MOD(IABPDG/100000,10)
        IF (L.EQ.0.AND.J.EQ.0.AND.N.EQ.0.OR.
     &      L.GT.3.OR. J.GT.4.OR .N.GT.4) THEN
          SWTEF(I)=1.
        ELSE
          SWTEF(I)=REPWT(L,J,N)**2
        ENDIF
      ELSE
C Not recognized
        SWTEF(I)=1.
      ENDIF
  80  CONTINUE
C Prepare tables for cluster decays, except flavourless light mesons
      LTMP=1
      NCDKS=0
      DO 120 I=1,89
C Store particles, flavour MAPF(I), noting highest spin and lowest mass
      WTMX=0.
      REMMN=1000.
      DO 90 J=21,NRES
      IF (VTOCDK(J).OR.IFLAV(J).NE.MAPF(I)) GOTO 90
      NCDKS=NCDKS+1
      IF (NCDKS.GT.NMXCDK) CALL HWWARN('HWURES',101,*999)
      NCLDK(NCDKS)=J
      CLDKWT(NCDKS)=TWO*RSPIN(J)+ONE
      IF (CLDKWT(NCDKS).GT.WTMX) WTMX=CLDKWT(NCDKS)
      IF (RMASS(J).LT.REMMN) THEN
        REMMN=RMASS(J)
        IMN=NCDKS
      ENDIF
  90  CONTINUE
      IF (NCDKS+1-LTMP.EQ.0) THEN
        WRITE(6,100) MAPF(I)
  100   FORMAT(1X,'No particles exist for a cluster with flavour, ',I4,
     &            ' to decay into')
        CALL HWWARN('HWURES',51,*120)
      ENDIF
C Set scaled spin weights
      RWTMX=1./WTMX
      DO 110 J=LTMP,NCDKS
  110 CLDKWT(J)=CLDKWT(J)*RWTMX
C Swap order if lightest hadron of given flavour not first
      IF (IMN.NE.LTMP) THEN
        ITMP=NCLDK(LTMP)
        WTMP=CLDKWT(LTMP)
        NCLDK(LTMP)=NCLDK(IMN)
        CLDKWT(LTMP)=CLDKWT(IMN)
        NCLDK(IMN)=ITMP
        CLDKWT(IMN)=WTMP
      ENDIF
C Set pointers etc
      LOCTMP(I)=LTMP
      RESTMP(I)=FLOAT(NCDKS+1-LTMP)
      LTMP=NCDKS+1
  120 CONTINUE
C Now do flavourless light mesons, allowing for mixing in weights
      WTMX=0.
      REMMN=1000.
      WTMX2=0.
      REMMN2=1000.
      NTMP=0
      DO 140 J=21,NRES
      IF (VTOCDK(J)) THEN
        GOTO 140
C Calculate mixing weight for (|uubar>+|ddbar>)/sqrt(2) component
      ELSEIF (IFLAV(J).EQ.11) THEN
        WT=1.
      ELSEIF (IFLAV(J).EQ.33) THEN
        IF (J.EQ.22) THEN
          WT=SIN(ETAMIX*PIFAC/180.+ATAN(SQRT(TWO)))**2
        ELSEIF (J.EQ.25) THEN
          WT=COS(ETAMIX*PIFAC/180.+ATAN(SQRT(TWO)))**2
        ELSEIF (J.EQ.24) THEN
          WT=SIN(PHIMIX*PIFAC/180.+ATAN(SQRT(TWO)))**2
        ELSEIF (J.EQ.56) THEN
          WT=COS(PHIMIX*PIFAC/180.+ATAN(SQRT(TWO)))**2
        ELSEIF (J.EQ.26) THEN
          WT=SIN(F2MIX*PIFAC/180.+ATAN(SQRT(TWO)))**2
        ELSEIF (J.EQ.58) THEN
          WT=COS(F2MIX*PIFAC/180.+ATAN(SQRT(TWO)))**2
        ELSEIF (J.EQ.28) THEN
          WT=SIN(F1MIX*PIFAC/180.+ATAN(SQRT(TWO)))**2
        ELSEIF (J.EQ.57) THEN
          WT=COS(F1MIX*PIFAC/180.+ATAN(SQRT(TWO)))**2
        ELSEIF (J.EQ.288) THEN
          WT=SIN(H1MIX*PIFAC/180.+ATAN(SQRT(TWO)))**2
        ELSEIF (J.EQ.289) THEN
          WT=COS(H1MIX*PIFAC/180.+ATAN(SQRT(TWO)))**2
        ELSEIF (J.EQ.293) THEN
          WT=SIN(F0MIX*PIFAC/180.+ATAN(SQRT(TWO)))**2
        ELSEIF (J.EQ.294) THEN
          WT=COS(F0MIX*PIFAC/180.+ATAN(SQRT(TWO)))**2
        ELSE
          WT=1./3.
          WRITE(6,130) J
  130   FORMAT(1X,'Particle ',I3,' not recognised, no I=0 mixing',
     &           ' assumed')
        ENDIF
      ELSE
        GOTO 140
      ENDIF
      IF (WT.GT.EPS) THEN
        NCDKS=NCDKS+1
        IF (NCDKS.GT.NMXCDK) CALL HWWARN('HWURES',102,*999)
        NCLDK(NCDKS)=J
        CLDKWT(NCDKS)=WT*(TWO*RSPIN(J)+ONE)
        IF (CLDKWT(NCDKS).GT.WTMX) WTMX=CLDKWT(NCDKS)
        IF (RMASS(J).LT.REMMN) THEN
          REMMN=RMASS(J)
          IMN=NCDKS
        ENDIF
      ENDIF
      IF (ONE-WT.GT.EPS) THEN
        NTMP=NTMP+1
        IF (NTMP.GT.NMXTMP) CALL HWWARN('HWURES',103,*999)
        NCDTMP(NTMP)=J
        CDWTMP(NTMP)=(ONE-WT)*(TWO*RSPIN(J)+ONE)
        IF (CDWTMP(NTMP).GT.WTMX2) WTMX2=CDWTMP(NTMP)
        IF (RMASS(J).LT.REMMN2) THEN
          REMMN2=RMASS(J)
          IMN2=NTMP
        ENDIF
      ENDIF
  140 CONTINUE
      IF (NCDKS+1-LTMP.EQ.0) THEN
        WRITE(6,100) 11
        CALL HWWARN('HWURES',52,*160)
      ENDIF
C Normalize scaled spin weights
      RWTMX=1./WTMX
      DO 150 I=LTMP,NCDKS
  150 CLDKWT(I)=CLDKWT(I)*RWTMX
C Swap order if lightest hadron of flavour 11 not first
      IF (IMN.NE.LTMP) THEN
        ITMP=NCLDK(LTMP)
        WTMP=CLDKWT(LTMP)
        NCLDK(LTMP)=NCLDK(IMN)
        CLDKWT(LTMP)=CLDKWT(IMN)
        NCLDK(IMN)=ITMP
        CLDKWT(IMN)=WTMP
      ENDIF
  160 IF (NTMP.EQ.0) THEN
        WRITE(6,100) 33
        CALL HWWARN('HWURES',53,*180)
      ENDIF
      IF (NCDKS+NTMP.GT.NMXCDK) CALL HWWARN('HWURES',104,*999)
C Store hadrons for |ssbar> channel and normalize their weights
      RWTMX=1./WTMX2
      DO 170 I=1,NTMP
      J=NCDKS+I
      NCLDK(J)=NCDTMP(I)
  170 CLDKWT(J)=CDWTMP(I)*RWTMX
C Swap order if lightest hadron of flavour 33 not first
      IF (IMN2.NE.1) THEN
        ITMP=NCLDK(NCDKS+1)
        WTMP=CLDKWT(NCDKS+1)
        NCLDK(NCDKS+1)=NCLDK(NCDKS+IMN2)
        CLDKWT(NCDKS+1)=CLDKWT(NCDKS+IMN2)
        NCLDK(NCDKS+IMN2)=ITMP
        CLDKWT(NCDKS+IMN2)=WTMP
      ENDIF
C Set pointers etc
  180 LOCTMP(90)=LTMP
      RESTMP(90)=FLOAT(NCDKS+1-LTMP)
      LOCTMP(91)=NCDKS+1
      RESTMP(91)=FLOAT(NTMP)
C Set pointers to hadrons of given flavours for cluster decays
      DO 190 I=1,12
      DO 190 J=1,12
      K=MAPC(I,J)
      IF (K.EQ.0) THEN
        RMIN(I,J)=RMASS(NCLDK(LOCN(I,2)))+RMASS(NCLDK(LOCN(I,2)))+1.D-2
      ELSE
        LOCN(I,J)=LOCTMP(K)
        RESN(I,J)=RESTMP(K)
        RMIN(I,J)=RMASS(NCLDK(LOCN(I,J)))
      ENDIF
  190 CONTINUE
  999 END
