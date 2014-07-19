CDECK  ID>, HWHEWW.
*CMZ :-        -02/05/91  10.58.29  by  Federico Carminati
*-- Author :    Zoltan Kunszt, modified by Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWHEWW
C-----------------------------------------------------------------------
C     E+E- -> W+W-/Z0Z0 (BASED ON ZOLTAN KUNSZT'S PROGRAM)
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      COMPLEX ZH,ZCH,ZD
      DOUBLE PRECISION HWUAEM,HWRGEN,HWUPCM,ETOT,STOT,FLUXW,GAMM,GIMM,
     & WM2,WXMIN,WX1MAX,WX2MAX,FJAC1,FJAC2,WX1,WX2,WMM1,WMM2,XXM,W2BO,
     & PST,WEIGHT,TOTSIG,WMASS,WWIDTH,ELST,CV,CA,BR,XMASS,PLAB,PRW,PCM,
     & AMPWW(4),CCC,HELSUM,HELCTY,BRZED(12),BRTOT,CPFAC,CPALL,RLL(12),
     & RRL(12),DIST(4)
      INTEGER IB,IBOS,I,ID1,ID2,NTRY,IDP(10),IDBOS(2),J1,J2,IPRC,ILST,
     & IDZOLT(16),MAP(12),NEWHEP
      LOGICAL EISBM1,HWRLOG
      EXTERNAL HWUAEM,HWRGEN,HWUPCM
      SAVE IDP,STOT,FLUXW,GAMM,GIMM,WM2,WXMIN,WX1MAX,FJAC1,ELST,ILST,
     & IDBOS,WMASS,WWIDTH
      COMMON/HWHEWP/XMASS(10),PLAB(5,10),PRW(5,2),PCM(5,10)
      COMMON/HWHEWQ/ZH(7,7),ZCH(7,7),ZD(7,7)
      COMMON/HWHEWR/CPFAC(12,12,8),CPALL(8)
      DATA ELST,ILST/0.,0/
      DATA IDZOLT/4,3,8,7,12,11,4*0,2,1,6,5,10,9/
      DATA MAP/12,11,2,1,14,13,4,3,16,15,6,5/
      IF (IERROR.NE.0) RETURN
      EISBM1=IDHW(1).LT.IDHW(2)
      IF (GENEV) THEN
        NEWHEP=NHEP
        NHEP=NHEP+2
        DO 20 IB=1,2
        IBOS=IB+NEWHEP
        CALL HWVEQU(5,PRW(1,IB),PHEP(1,IBOS))
        IF (EISBM1) PHEP(3,IBOS)=-PHEP(3,IBOS)
        CALL HWVZRO(4,VHEP(1,IBOS))
        CALL HWUDKL(IDBOS(IB),PHEP(1,IBOS),DIST)
        CALL HWVSUM(4,VHEP(1,IBOS),DIST,DIST)
        IDHW(IBOS)=IDBOS(IB)
        IDHEP(IBOS)=IDPDG(IDBOS(IB))
        JMOHEP(1,IBOS)=1
        JMOHEP(2,IBOS)=2
        ISTHEP(IBOS)=110
        DO 10 I=1,2
          CALL HWVEQU(5,PLAB(1,2*IB+I),PHEP(1,NHEP+I))
          IF (EISBM1) PHEP(3,NHEP+I)=-PHEP(3,NHEP+I)
          CALL HWVEQU(4,DIST,VHEP(1,NHEP+I))
C---STATUS, IDs AND POINTERS
          ISTHEP(NHEP+I)=112+I
          IDHW(NHEP+I)=IDP(2*IB+I)
          IDHEP(NHEP+I)=IDPDG(IDP(2*IB+I))
          JDAHEP(I,IBOS)=NHEP+I
          JMOHEP(1,NHEP+I)=IBOS
          JMOHEP(2,NHEP+I)=JMOHEP(1,IBOS)
 10     CONTINUE
        NHEP=NHEP+2
        JMOHEP(2,NHEP)=NHEP-1
        JDAHEP(2,NHEP)=NHEP-1
        JMOHEP(2,NHEP-1)=NHEP
        JDAHEP(2,NHEP-1)=NHEP
 20     CONTINUE
      ELSE
        EMSCA=PHEP(5,3)
        ETOT=EMSCA
        IPRC=MOD(IPROC,100)
        IF (ETOT.NE.ELST .OR. IPRC.NE.ILST) THEN
          STOT=ETOT*ETOT
          FLUXW=GEV2NB*.125*(HWUAEM(STOT)/PIFAC)**4/STOT
          IF (IPRC.EQ.0) THEN
            WMASS=RMASS(198)
            WWIDTH=GAMW
            IDBOS(1)=198
            IDBOS(2)=199
          ELSEIF (IPRC.EQ.50) THEN
            WMASS=RMASS(200)
            WWIDTH=GAMZ
            IDBOS(1)=200
            IDBOS(2)=200
C---LOAD FERMION COUPLINGS TO Z
            DO 30 I=1,12
              RLL(I)=VFCH(MAP(I),1)+AFCH(MAP(I),1)
              RRL(I)=VFCH(MAP(I),1)-AFCH(MAP(I),1)
 30         CONTINUE
            RLL(11)=0
            RRL(11)=0
            BRTOT=0
            DO 60 J1=1,12
              BRZED(J1)=0
              DO 50 J2=1,12
                CCC=1
                IF (MOD(J1-1,4).GE.2) CCC=CCC*CAFAC
                IF (MOD(J2-1,4).GE.2) CCC=CCC*CAFAC
                CPFAC(J1,J2,1)=CCC*(RLL(2)**2*RLL(J1)*RLL(J2))**2
                CPFAC(J1,J2,2)=CCC*(RLL(2)**2*RRL(J1)*RLL(J2))**2
                CPFAC(J1,J2,3)=CCC*(RLL(2)**2*RLL(J1)*RRL(J2))**2
                CPFAC(J1,J2,4)=CCC*(RLL(2)**2*RRL(J1)*RRL(J2))**2
                CPFAC(J1,J2,5)=CCC*(RRL(2)**2*RLL(J1)*RLL(J2))**2
                CPFAC(J1,J2,6)=CCC*(RRL(2)**2*RRL(J1)*RLL(J2))**2
                CPFAC(J1,J2,7)=CCC*(RRL(2)**2*RLL(J1)*RRL(J2))**2
                CPFAC(J1,J2,8)=CCC*(RRL(2)**2*RRL(J1)*RRL(J2))**2
                DO 40 I=1,8
                  IF (J1.EQ.1.AND.J2.EQ.1) CPALL(I)=0
                  CPALL(I)=CPALL(I)+CPFAC(J1,J2,I)
                  BRZED(J1)=BRZED(J1)+CPFAC(J1,J2,I)
                  BRTOT=BRTOT+CPFAC(J1,J2,I)
 40             CONTINUE
 50           CONTINUE
 60         CONTINUE
            DO 70 I=1,12
 70           BRZED(I)=BRZED(I)/BRTOT
          ELSE
            CALL HWWARN('HWHEWW',500,*999)
          ENDIF
          GAMM=WMASS*WWIDTH
          GIMM=1.D0/GAMM
          WM2=WMASS*WMASS
          WXMIN=ATAN(-WMASS/WWIDTH)
          WX1MAX=ATAN((STOT-WM2)*GIMM)
          FJAC1=WX1MAX-WXMIN
          ILST=IPRC
          ELST=ETOT
        ENDIF
        EVWGT=0
C---CHOOSE W MASSES
        WX1=WXMIN+FJAC1*HWRGEN(1)
        WMM1=GAMM*TAN(WX1)+WM2
        XMASS(1)=SQRT(WMM1)
        WX2MAX=ATAN(((ETOT-XMASS(1))**2-WM2)*GIMM)
        FJAC2=WX2MAX-WXMIN
        WX2=WXMIN+FJAC2*HWRGEN(2)
        WMM2=GAMM*TAN(WX2)+WM2
        XMASS(2)=SQRT(WMM2)
        IF (HWRLOG(HALF))THEN
         XXM=XMASS(1)
         XMASS(1)=XMASS(2)
         XMASS(2)=XXM
        ENDIF
C---CTMAX=ANGULAR CUT ON COS W-ANGLE
        CALL HWHEW0(1,ETOT,XMASS(1),PRW(1,1),W2BO,CTMAX)
        IF (W2BO.EQ.0) RETURN
C---FOR ZZ EVENTS, FORCE BOSE STATISTICS, BY KILLING EVENTS WITH COS1<0
        IF (IPRC.NE.0) THEN
          IF (PRW(3,1).LT.0) RETURN
C---AND THEN SYMMETRIZE (THIS PROCEDURE VASTLY IMPROVES EFFICIENCY)
          IF (HWRLOG(HALF)) THEN
            PRW(3,1)=-PRW(3,1)
            PRW(3,2)=-PRW(3,2)
          ENDIF
        ENDIF
        PLAB(3,1)=0.5*ETOT
        PLAB(4,1)=PLAB(3,1)
        PLAB(3,2)=-PLAB(3,1)
        PLAB(4,2)=PLAB(3,1)
C
C---LET THE W BOSONS DECAY
        NTRY=0
 80     NTRY=NTRY+1
        DO 90 IB=1,2
        CALL HWDBOZ(IDBOS(IB),ID1,ID2,CV,CA,BR,1)
        PST=HWUPCM(XMASS(IB),RMASS(ID1),RMASS(ID2))
        IF (PST.LT.0.) THEN
          CALL HWDBOZ(IDBOS(IB),ID1,ID2,CV,CA,BR,2)
          IF (NTRY.LE.NBTRY) GOTO 80
C          CALL HWWARN('HWHEWW',1,*999)
          RETURN
        ENDIF
        PRW(5,IB)=XMASS(IB)
        IDP(2*IB+1)=ID1
        IDP(2*IB+2)=ID2
        PLAB(5,2*IB+1)=RMASS(ID1)
        PLAB(5,2*IB+2)=RMASS(ID2)
        CALL HWDTWO(PRW(1,IB),PLAB(1,2*IB+1),PLAB(1,2*IB+2),
     &              PST,TWO,.TRUE.)
 90     CONTINUE
        WEIGHT=FLUXW*W2BO*FJAC1*FJAC2*(0.5D0*PIFAC*GIMM)**2
        CALL HWHEW1(6)
        CALL HWHEW2(6,PCM(1,1),ZH,ZCH,ZD)
        IF (IPRC.EQ.0) THEN
          CALL HWHEW3(5,6,3,4,1,2,AMPWW)
          TOTSIG=9.*AMPWW(1)+6.*(AMPWW(2)+AMPWW(3))+4.*AMPWW(4)
          EVWGT=TOTSIG*WEIGHT*BR
        ELSE
          ID1=IDZOLT(IDPDG(IDP(3)))
          ID2=IDZOLT(IDPDG(IDP(5)))
          CALL HWHEW5(5,6,3,4,1,2,HELSUM,HELCTY,ID1,ID2)
          EVWGT=HELCTY*WEIGHT*BR/(BRZED(ID1)*BRZED(ID2))
        ENDIF
      ENDIF
 999  END
