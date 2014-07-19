*
* $Id: pywidt.F,v 1.1.1.1 1996/01/11 14:05:28 mclareni Exp $
*
* $Log: pywidt.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:28  mclareni
* Fritiof
*
*
C*********************************************************************
 
      SUBROUTINE PYWIDT(KFLR,SH,WDTP,WDTE)
 
C...Calculates full and partial widths of resonances.
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYINT1/MINT(400),VINT(400)
      COMMON/PYINT4/WIDP(21:40,0:40),WIDE(21:40,0:40),WIDS(21:40,3)
      SAVE /LUDAT1/,/LUDAT2/,/LUDAT3/
      SAVE /PYSUBS/,/PYPARS/,/PYINT1/,/PYINT4/
      DIMENSION WDTP(0:40),WDTE(0:40,0:5),MOFSV(3,2),WIDWSV(3,2),
     &WID2SV(3,2)
      SAVE MOFSV,WIDWSV,WID2SV
      DATA MOFSV/6*0/,WIDWSV/6*0./,WID2SV/6*0./
 
C...Some common constants.
      KFLA=IABS(KFLR)
      KFHIGG=25
      IHIGG=1
      IF(KFLA.EQ.35.OR.KFLA.EQ.36) THEN
        KFHIGG=KFLA
        IHIGG=KFLA-33
      ENDIF
      AEM=ULALEM(SH)
      XW=PARU(102)
      AS=ULALPS(SH)
      RADC=1.+AS/PARU(1)
 
C...Reset width information.
      DO 100 I=0,40
      WDTP(I)=0.
      DO 100 J=0,5
  100 WDTE(I,J)=0.
 
      IF(KFLA.EQ.21) THEN
C...QCD:
        DO 110 I=1,MDCY(21,3)
        IDC=I+MDCY(21,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 110
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 110
        IF(I.LE.8) THEN
C...QCD -> q + q~
          WDTP(I)=(1.+2.*RM1)*SQRT(MAX(0.,1.-4.*RM1))
          WID2=1.
        ENDIF
        WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
          WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
          WDTE(I,0)=WDTE(I,MDME(IDC,1))
          WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
        ENDIF
  110   CONTINUE
 
      ELSEIF(KFLA.EQ.22) THEN
C...QED photon.
        DO 120 I=1,MDCY(22,3)
        IDC=I+MDCY(22,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 120
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 120
        IF(I.LE.8) THEN
C...QED -> q + q~.
          EF=KCHG(I,1)/3.
          FCOF=3.*RADC
          WDTP(I)=FCOF*EF**2*(1.+2.*RM1)*SQRT(MAX(0.,1.-4.*RM1))
          WID2=1.
        ELSEIF(I.LE.12) THEN
C...QED -> l+ + l-.
          EF=KCHG(9+2*(I-8),1)/3.
          WDTP(I)=EF**2*(1.+2.*RM1)*SQRT(MAX(0.,1.-4.*RM1))
          WID2=1.
        ENDIF
        WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
          WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
          WDTE(I,0)=WDTE(I,MDME(IDC,1))
          WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
        ENDIF
  120   CONTINUE
 
      ELSEIF(KFLA.EQ.23) THEN
C...Z0:
        ICASE=1
        XWC=1./(16.*XW*(1.-XW))
        FACH=AEM/3.*XWC*SH
  130   CONTINUE
        IF(MINT(61).GE.1.AND.ICASE.EQ.2) THEN
          VINT(111)=0.
          VINT(112)=0.
          VINT(114)=0.
        ENDIF
        IF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
          EI=KCHG(IABS(MINT(15)),1)/3.
          AI=SIGN(1.,EI)
          VI=AI-4.*EI*XW
          SQMZ=PMAS(23,1)**2
          HZ=FACH*WDTP(0)
          IF(MSTP(43).EQ.1.OR.MSTP(43).EQ.3) VINT(111)=1.
          IF(MSTP(43).EQ.3) VINT(112)=
     &    2.*XWC*SH*(SH-SQMZ)/((SH-SQMZ)**2+HZ**2)
          IF(MSTP(43).EQ.2.OR.MSTP(43).EQ.3) VINT(114)=
     &    XWC**2*SH**2/((SH-SQMZ)**2+HZ**2)
        ENDIF
        DO 140 I=1,MDCY(23,3)
        IDC=I+MDCY(23,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 140
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 140
        IF(I.LE.8) THEN
C...Z0 -> q + q~
          EF=KCHG(I,1)/3.
          AF=SIGN(1.,EF+0.1)
          VF=AF-4.*EF*XW
          FCOF=3.*RADC
        ELSEIF(I.LE.16) THEN
C...Z0 -> l+ + l-, nu + nu~
          EF=KCHG(I+2,1)/3.
          AF=SIGN(1.,EF+0.1)
          VF=AF-4.*EF*XW
          FCOF=1.
        ENDIF
        BE34=SQRT(MAX(0.,1.-4.*RM1))
        IF(ICASE.EQ.1) THEN
          WDTP(I)=FCOF*(VF**2*(1.+2.*RM1)+AF**2*(1.-4.*RM1))*BE34
        ELSEIF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
          WDTP(I)=FCOF*((EI**2*VINT(111)*EF**2+EI*VI*VINT(112)*
     &    EF*VF+(VI**2+AI**2)*VINT(114)*VF**2)*(1.+2.*RM1)+
     &    (VI**2+AI**2)*VINT(114)*AF**2*(1.-4.*RM1))*BE34
        ELSEIF(MINT(61).EQ.2.AND.ICASE.EQ.2) THEN
          FGGF=FCOF*EF**2*(1.+2.*RM1)*BE34
          FGZF=FCOF*EF*VF*(1.+2.*RM1)*BE34
          FZZF=FCOF*(VF**2*(1.+2.*RM1)+AF**2*(1.-4.*RM1))*BE34
        ENDIF
        WID2=1.
        IF(ICASE.EQ.1) WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          IF((ICASE.EQ.1.AND.MINT(61).NE.1).OR.
     &    (ICASE.EQ.2.AND.MINT(61).EQ.1)) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
          IF(MINT(61).EQ.2.AND.ICASE.EQ.2) THEN
            IF(MSTP(43).EQ.1.OR.MSTP(43).EQ.3) VINT(111)=
     &      VINT(111)+FGGF*WID2
            IF(MSTP(43).EQ.3) VINT(112)=VINT(112)+FGZF*WID2
            IF(MSTP(43).EQ.2.OR.MSTP(43).EQ.3) VINT(114)=
     &      VINT(114)+FZZF*WID2
          ENDIF
        ENDIF
  140   CONTINUE
        IF(MINT(61).GE.1) ICASE=3-ICASE
        IF(ICASE.EQ.2) GOTO 130
 
      ELSEIF(KFLA.EQ.24) THEN
C...W+/-:
        DO 150 I=1,MDCY(24,3)
        IDC=I+MDCY(24,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 150
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 150
        IF(I.LE.16) THEN
C...W+/- -> q + q~'
          FCOF=3.*RADC*VCKM((I-1)/4+1,MOD(I-1,4)+1)
        ELSEIF(I.LE.20) THEN
C...W+/- -> l+/- + nu
          FCOF=1.
        ENDIF
        WDTP(I)=FCOF*(2.-RM1-RM2-(RM1-RM2)**2)*
     &  SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))
        WID2=1.
        WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
          WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
          WDTE(I,0)=WDTE(I,MDME(IDC,1))
          WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
        ENDIF
  150   CONTINUE
 
      ELSEIF(KFLA.EQ.25.OR.KFLA.EQ.35.OR.KFLA.EQ.36) THEN
C...H0 (or H'0, or A0):
        DO 190 I=1,MDCY(KFHIGG,3)
        IDC=I+MDCY(KFHIGG,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 190
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(I.NE.16.AND.I.NE.17.AND.SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 190
        IF(I.LE.8) THEN
C...H0 -> q + q~
          WDTP(I)=3.*RM1*(1.-4.*RM1)*SQRT(MAX(0.,1.-4.*RM1))*RADC
          IF(MSTP(37).EQ.1) WDTP(I)=WDTP(I)*
     &    (LOG(MAX(4.,PARP(37)**2*RM1*SH/PARU(117)**2))/
     &    LOG(MAX(4.,SH/PARU(117)**2)))**(24./(33.-2.*MSTU(118)))
          IF(MSTP(4).GE.1.OR.IHIGG.GE.2) THEN
            IF(MOD(I,2).EQ.1) WDTP(I)=WDTP(I)*PARU(151+10*IHIGG)**2
            IF(MOD(I,2).EQ.0) WDTP(I)=WDTP(I)*PARU(152+10*IHIGG)**2
          ENDIF
          WID2=1.
        ELSEIF(I.LE.12) THEN
C...H0 -> l+ + l-
          WDTP(I)=RM1*(1.-4.*RM1)*SQRT(MAX(0.,1.-4.*RM1))
          IF(MSTP(4).GE.1.OR.IHIGG.GE.2) WDTP(I)=WDTP(I)*
     &    PARU(153+10*IHIGG)**2
          WID2=1.
        ELSEIF(I.EQ.13) THEN
C...H0 -> g + g; quark loop contribution only
          ETARE=0.
          ETAIM=0.
          DO 160 J=1,2*MSTP(1)
          EPS=(2.*PMAS(J,1))**2/SH
          IF(EPS.LE.1.) THEN
            IF(EPS.GT.1.E-4) THEN
              ROOT=SQRT(1.-EPS)
              RLN=LOG((1.+ROOT)/(1.-ROOT))
            ELSE
              RLN=LOG(4./EPS-2.)
            ENDIF
            PHIRE=0.25*(RLN**2-PARU(1)**2)
            PHIIM=0.5*PARU(1)*RLN
          ELSE
            PHIRE=-(ASIN(1./SQRT(EPS)))**2
            PHIIM=0.
          ENDIF
          ETAREJ=0.5*EPS*(1.+(EPS-1.)*PHIRE)
          ETAIMJ=0.5*EPS*(EPS-1.)*PHIIM
          IF(MSTP(4).GE.1.OR.IHIGG.GE.2.AND.MOD(J,2).EQ.1) THEN
            ETAREJ=ETAREJ*PARU(151+10*IHIGG)
            ETAIMJ=ETAIMJ*PARU(151+10*IHIGG)
          ELSEIF(MSTP(4).GE.1.OR.IHIGG.GE.2) THEN
            ETAREJ=ETAREJ*PARU(152+10*IHIGG)
            ETAIMJ=ETAIMJ*PARU(152+10*IHIGG)
          ENDIF
          ETARE=ETARE+ETAREJ
          ETAIM=ETAIM+ETAIMJ
  160     CONTINUE
          ETA2=ETARE**2+ETAIM**2
          WDTP(I)=(AS/PARU(1))**2*ETA2
          WID2=1.
        ELSEIF(I.EQ.14) THEN
C...H0 -> gamma + gamma; quark, charged lepton and W loop contributions
          ETARE=0.
          ETAIM=0.
          DO 170 J=1,3*MSTP(1)+1
          IF(J.LE.2*MSTP(1)) THEN
            EJ=KCHG(J,1)/3.
            EPS=(2.*PMAS(J,1))**2/SH
          ELSEIF(J.LE.3*MSTP(1)) THEN
            JL=2*(J-2*MSTP(1))-1
            EJ=KCHG(10+JL,1)/3.
            EPS=(2.*PMAS(10+JL,1))**2/SH
          ELSE
            EPS=(2.*PMAS(24,1))**2/SH
          ENDIF
          IF(EPS.LE.1.) THEN
            IF(EPS.GT.1.E-4) THEN
              ROOT=SQRT(1.-EPS)
              RLN=LOG((1.+ROOT)/(1.-ROOT))
            ELSE
              RLN=LOG(4./EPS-2.)
            ENDIF
            PHIRE=0.25*(RLN**2-PARU(1)**2)
            PHIIM=0.5*PARU(1)*RLN
          ELSE
            PHIRE=-(ASIN(1./SQRT(EPS)))**2
            PHIIM=0.
          ENDIF
          IF(J.LE.2*MSTP(1)) THEN
            ETAREJ=0.5*3.*EJ**2*EPS*(1.+(EPS-1.)*PHIRE)
            ETAIMJ=0.5*3.*EJ**2*EPS*(EPS-1.)*PHIIM
            IF(MSTP(4).GE.1.OR.IHIGG.GE.2.AND.MOD(J,2).EQ.1) THEN
              ETAREJ=ETAREJ*PARU(151+10*IHIGG)
              ETAIMJ=ETAIMJ*PARU(151+10*IHIGG)
            ELSEIF(MSTP(4).GE.1.OR.IHIGG.GE.2) THEN
              ETAREJ=ETAREJ*PARU(152+10*IHIGG)
              ETAIMJ=ETAIMJ*PARU(152+10*IHIGG)
            ENDIF
          ELSEIF(J.LE.3*MSTP(1)) THEN
            ETAREJ=0.5*EJ**2*EPS*(1.+(EPS-1.)*PHIRE)
            ETAIMJ=0.5*EJ**2*EPS*(EPS-1.)*PHIIM
            IF(MSTP(4).GE.1.OR.IHIGG.GE.2) THEN
              ETAREJ=ETAREJ*PARU(153+10*IHIGG)
              ETAIMJ=ETAIMJ*PARU(153+10*IHIGG)
            ENDIF
          ELSE
            ETAREJ=-0.5-0.75*EPS*(1.+(EPS-2.)*PHIRE)
            ETAIMJ=0.75*EPS*(EPS-2.)*PHIIM
            IF(MSTP(4).GE.1.OR.IHIGG.GE.2) THEN
              ETAREJ=ETAREJ*PARU(155+10*IHIGG)
              ETAIMJ=ETAIMJ*PARU(155+10*IHIGG)
            ENDIF
          ENDIF
          ETARE=ETARE+ETAREJ
          ETAIM=ETAIM+ETAIMJ
  170     CONTINUE
          ETA2=ETARE**2+ETAIM**2
          WDTP(I)=(AEM/PARU(1))**2*0.5*ETA2
          WID2=1.
        ELSEIF(I.EQ.15) THEN
C...H0 -> gamma + Z0; quark, charged lepton and W loop contributions
          ETARE=0.
          ETAIM=0.
          DO 180 J=1,3*MSTP(1)+1
          IF(J.LE.2*MSTP(1)) THEN
            EJ=KCHG(J,1)/3.
            AJ=SIGN(1.,EJ+0.1)
            VJ=AJ-4.*EJ*XW
            EPS=(2.*PMAS(J,1))**2/SH
            EPSP=(2.*PMAS(J,1)/PMAS(23,1))**2
          ELSEIF(J.LE.3*MSTP(1)) THEN
            JL=2*(J-2*MSTP(1))-1
            EJ=KCHG(10+JL,1)/3.
            AJ=SIGN(1.,EJ+0.1)
            VJ=AJ-4.*EJ*XW
            EPS=(2.*PMAS(10+JL,1))**2/SH
            EPSP=(2.*PMAS(10+JL,1)/PMAS(23,1))**2
          ELSE
            EPS=(2.*PMAS(24,1))**2/SH
            EPSP=(2.*PMAS(24,1)/PMAS(23,1))**2
          ENDIF
          IF(EPS.LE.1.) THEN
            ROOT=SQRT(1.-EPS)
            IF(EPS.GT.1.E-4) THEN
              RLN=LOG((1.+ROOT)/(1.-ROOT))
            ELSE
              RLN=LOG(4./EPS-2.)
            ENDIF
            PHIRE=0.25*(RLN**2-PARU(1)**2)
            PHIIM=0.5*PARU(1)*RLN
            PSIRE=-(1.+0.5*ROOT*RLN)
            PSIIM=0.5*PARU(1)*ROOT
          ELSE
            PHIRE=-(ASIN(1./SQRT(EPS)))**2
            PHIIM=0.
            PSIRE=-(1.+SQRT(EPS-1.)*ASIN(1./SQRT(EPS)))
            PSIIM=0.
          ENDIF
          IF(EPSP.LE.1.) THEN
            ROOT=SQRT(1.-EPSP)
            IF(EPSP.GT.1.E-4) THEN
              RLN=LOG((1.+ROOT)/(1.-ROOT))
            ELSE
              RLN=LOG(4./EPSP-2.)
            ENDIF
            PHIREP=0.25*(RLN**2-PARU(1)**2)
            PHIIMP=0.5*PARU(1)*RLN
            PSIREP=-(1.+0.5*ROOT*RLN)
            PSIIMP=0.5*PARU(1)*ROOT
          ELSE
            PHIREP=-(ASIN(1./SQRT(EPSP)))**2
            PHIIMP=0.
            PSIREP=-(1.+SQRT(EPSP-1.)*ASIN(1./SQRT(EPSP)))
            PSIIMP=0.
          ENDIF
          FXYRE=EPS*EPSP/(8.*(EPS-EPSP))*(1.-EPS*EPSP/(EPS-EPSP)*(PHIRE-
     &    PHIREP)-2.*EPS/(EPS-EPSP)*(PSIRE-PSIREP))
          FXYIM=EPS*EPSP/(8.*(EPS-EPSP))*(-EPS*EPSP/(EPS-EPSP)*(PHIIM-
     &    PHIIMP)-2.*EPS/(EPS-EPSP)*(PSIIM-PSIIMP))
          F1RE=EPS*EPSP/(2.*(EPS-EPSP))*(PHIRE-PHIREP)
          F1IM=EPS*EPSP/(2.*(EPS-EPSP))*(PHIIM-PHIIMP)
          IF(J.LE.2*MSTP(1)) THEN
            ETAREJ=-3.*EJ*VJ*(FXYRE-0.25*F1RE)
            ETAIMJ=-3.*EJ*VJ*(FXYIM-0.25*F1IM)
            IF(MSTP(4).GE.1.OR.IHIGG.GE.2.AND.MOD(J,2).EQ.1) THEN
              ETAREJ=ETAREJ*PARU(151+10*IHIGG)
              ETAIMJ=ETAIMJ*PARU(151+10*IHIGG)
            ELSEIF(MSTP(4).GE.1.OR.IHIGG.GE.2) THEN
              ETAREJ=ETAREJ*PARU(152+10*IHIGG)
              ETAIMJ=ETAIMJ*PARU(152+10*IHIGG)
            ENDIF
          ELSEIF(J.LE.3*MSTP(1)) THEN
            ETAREJ=-EJ*VJ*(FXYRE-0.25*F1RE)
            ETAIMJ=-EJ*VJ*(FXYIM-0.25*F1IM)
            IF(MSTP(4).GE.1.OR.IHIGG.GE.2) THEN
              ETAREJ=ETAREJ*PARU(153+10*IHIGG)
              ETAIMJ=ETAIMJ*PARU(153+10*IHIGG)
            ENDIF
          ELSE
            ETAREJ=-(1.-XW)*(((1.+2./EPS)*XW/(1.-XW)-
     &      (5.+2./EPS))*FXYRE+(3.-XW/(1.-XW))*F1RE)
            ETAIMJ=-(1.-XW)*(((1.+2./EPS)*XW/(1.-XW)-
     &      (5.+2./EPS))*FXYIM+(3.-XW/(1.-XW))*F1IM)
            IF(MSTP(4).GE.1.OR.IHIGG.GE.2) THEN
              ETAREJ=ETAREJ*PARU(155+10*IHIGG)
              ETAIMJ=ETAIMJ*PARU(155+10*IHIGG)
            ENDIF
          ENDIF
          ETARE=ETARE+ETAREJ
          ETAIM=ETAIM+ETAIMJ
  180     CONTINUE
          ETA2=(ETARE**2+ETAIM**2)/(1.-XW)
          WDTP(I)=(AEM/PARU(1))**2*(1.-PMAS(23,1)**2/SH)**3/XW*ETA2
          WID2=WIDS(23,2)
        ELSEIF(I.LE.17) THEN
C...H0 -> Z0 + Z0, W+ + W-
          PM1=PMAS(IABS(KFDP(IDC,1)),1)
          PG1=PMAS(IABS(KFDP(IDC,1)),2)
          IF(MINT(62).GE.1) THEN
            IF(MSTP(42).EQ.0.OR.(4.*(PM1+10.*PG1)**2.LT.SH.AND.
     &      CKIN(46).LT.CKIN(45).AND.CKIN(48).LT.CKIN(47).AND.
     &      MAX(CKIN(45),CKIN(47)).LT.PM1-10.*PG1)) THEN
              MOFSV(IHIGG,I-15)=0
              WIDW=(1.-4.*RM1+12.*RM1**2)*SQRT(MAX(0.,1.-4.*RM1))
              WID2=1.
            ELSE
              MOFSV(IHIGG,I-15)=1
              RMAS=SQRT(MAX(0.,SH))
              CALL PYOFSH(1,KFLA,KFDP(IDC,1),KFDP(IDC,2),RMAS,WIDW,WID2)
              WIDWSV(IHIGG,I-15)=WIDW
              WID2SV(IHIGG,I-15)=WID2
            ENDIF
          ELSE
            IF(MOFSV(IHIGG,I-15).EQ.0) THEN
              WIDW=(1.-4.*RM1+12.*RM1**2)*SQRT(MAX(0.,1.-4.*RM1))
              WID2=1.
            ELSE
              WIDW=WIDWSV(IHIGG,I-15)
              WID2=WID2SV(IHIGG,I-15)
            ENDIF
          ENDIF
          WDTP(I)=WIDW/(2.*(18-I))
          IF(MSTP(4).GE.1.OR.IHIGG.GE.2) WDTP(I)=WDTP(I)*
     &    PARU(138+I+10*IHIGG)**2
          WID2=WID2*WIDS(7+I,1)
        ELSEIF(I.EQ.18.AND.KFLA.EQ.35) THEN
C...H'0 -> H0 + H0.
          WDTP(I)=PARU(176)**2*0.25*PMAS(23,1)**4/SH**2*
     &    SQRT(MAX(0.,1.-4.*RM1))
          WID2=WIDS(25,2)**2
        ELSEIF(I.EQ.19.AND.KFLA.EQ.35) THEN
C...H'0 -> A0 + A0.
          WDTP(I)=PARU(177)**2*0.25*PMAS(23,1)**4/SH**2*
     &    SQRT(MAX(0.,1.-4.*RM1))
          WID2=WIDS(36,2)**2
        ELSEIF(I.EQ.18.AND.KFLA.EQ.36) THEN
C...A0 -> Z0 + H0.
          WDTP(I)=PARU(186)**2*0.5*SQRT(MAX(0.,(1.-RM1-RM2)**2-
     &    4.*RM1*RM2))**3
          WID2=WIDS(23,2)*WIDS(25,2)
        ENDIF
        WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
          WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
          WDTE(I,0)=WDTE(I,MDME(IDC,1))
          WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
        ENDIF
  190   CONTINUE
 
      ELSEIF(KFLA.EQ.32) THEN
C...Z'0:
        ICASE=1
        XWC=1./(16.*XW*(1.-XW))
        FACH=AEM/3.*XWC*SH
        VINT(117)=0.
  200   CONTINUE
        IF(MINT(61).GE.1.AND.ICASE.EQ.2) THEN
          VINT(111)=0.
          VINT(112)=0.
          VINT(113)=0.
          VINT(114)=0.
          VINT(115)=0.
          VINT(116)=0.
        ENDIF
        IF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
          KFAI=IABS(MINT(15))
          EI=KCHG(KFAI,1)/3.
          AI=SIGN(1.,EI+0.1)
          VI=AI-4.*EI*XW
          KFAIC=1
          IF(KFAI.LE.10.AND.MOD(KFAI,2).EQ.0) KFAIC=2
          IF(KFAI.GT.10.AND.MOD(KFAI,2).NE.0) KFAIC=3
          IF(KFAI.GT.10.AND.MOD(KFAI,2).EQ.0) KFAIC=4
          VPI=PARU(119+2*KFAIC)
          API=PARU(120+2*KFAIC)
          SQMZ=PMAS(23,1)**2
          HZ=FACH*VINT(117)
          SQMZP=PMAS(32,1)**2
          HZP=FACH*WDTP(0)
          IF(MSTP(44).EQ.1.OR.MSTP(44).EQ.4.OR.MSTP(44).EQ.5.OR.
     &    MSTP(44).EQ.7) VINT(111)=1.
          IF(MSTP(44).EQ.4.OR.MSTP(44).EQ.7) VINT(112)=
     &    2.*XWC*SH*(SH-SQMZ)/((SH-SQMZ)**2+HZ**2)
          IF(MSTP(44).EQ.5.OR.MSTP(44).EQ.7) VINT(113)=
     &    2.*XWC*SH*(SH-SQMZP)/((SH-SQMZP)**2+HZP**2)
          IF(MSTP(44).EQ.2.OR.MSTP(44).EQ.4.OR.MSTP(44).EQ.6.OR.
     &    MSTP(44).EQ.7) VINT(114)=XWC**2*SH**2/((SH-SQMZ)**2+HZ**2)
          IF(MSTP(44).EQ.6.OR.MSTP(44).EQ.7) VINT(115)=
     &    2.*XWC**2*SH**2*((SH-SQMZ)*(SH-SQMZP)+HZ*HZP)/
     &    (((SH-SQMZ)**2+HZ**2)*((SH-SQMZP)**2+HZP**2))
          IF(MSTP(44).EQ.3.OR.MSTP(44).EQ.5.OR.MSTP(44).EQ.6.OR.
     &    MSTP(44).EQ.7) VINT(116)=XWC**2*SH**2/((SH-SQMZP)**2+HZP**2)
        ENDIF
        DO 210 I=1,MDCY(32,3)
        IDC=I+MDCY(32,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 210
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1..OR.MDME(IDC,1).LT.0) GOTO 210
        IF(I.LE.16) THEN
          IF(I.LE.8) THEN
C...Z'0 -> q + q~
            EF=KCHG(I,1)/3.
            AF=SIGN(1.,EF+0.1)
            VF=AF-4.*EF*XW
            VPF=PARU(123-2*MOD(I,2))
            APF=PARU(124-2*MOD(I,2))
            FCOF=3.*RADC
          ELSEIF(I.LE.16) THEN
C...Z'0 -> l+ + l-, nu + nu~
            EF=KCHG(I+2,1)/3.
            AF=SIGN(1.,EF+0.1)
            VF=AF-4.*EF*XW
            VPF=PARU(127-2*MOD(I,2))
            APF=PARU(128-2*MOD(I,2))
            FCOF=1.
          ENDIF
          BE34=SQRT(MAX(0.,1.-4.*RM1))
          IF(ICASE.EQ.1) THEN
            WDTPZ=FCOF*(VF**2*(1.+2.*RM1)+AF**2*(1.-4.*RM1))*BE34
            WDTP(I)=FCOF*(VPF**2*(1.+2.*RM1)+APF**2*(1.-4.*RM1))*BE34
          ELSEIF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
            WDTP(I)=FCOF*((EI**2*VINT(111)*EF**2+EI*VI*VINT(112)*
     &      EF*VF+EI*VPI*VINT(113)*EF*VPF+(VI**2+AI**2)*VINT(114)*
     &      VF**2+(VI*VPI+AI*API)*VINT(115)*VF*VPF+(VPI**2+API**2)*
     &      VINT(116)*VPF**2)*(1.+2.*RM1)+((VI**2+AI**2)*VINT(114)*
     &      AF**2+(VI*VPI+AI*API)*VINT(115)*AF*APF+(VPI**2+API**2)*
     &      VINT(116)*APF**2)*(1.-4.*RM1))*BE34
          ELSEIF(MINT(61).EQ.2) THEN
            FGGF=FCOF*EF**2*(1.+2.*RM1)*BE34
            FGZF=FCOF*EF*VF*(1.+2.*RM1)*BE34
            FGZPF=FCOF*EF*VPF*(1.+2.*RM1)*BE34
            FZZF=FCOF*(VF**2*(1.+2.*RM1)+AF**2*(1.-4.*RM1))*BE34
            FZZPF=FCOF*(VF*VPF*(1.+2.*RM1)+AF*APF*(1.-4.*RM1))*BE34
            FZPZPF=FCOF*(VPF**2*(1.+2.*RM1)+APF**2*(1.-4.*RM1))*BE34
          ENDIF
          WID2=1.
        ELSEIF(I.EQ.17) THEN
C...Z'0 -> W+ + W-
          WDTPZP=PARU(129)**2*(1.-XW)**2*
     &    SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))**3*
     &    (1.+10.*RM1+10.*RM2+RM1**2+RM2**2+10.*RM1*RM2)
          IF(ICASE.EQ.1) THEN
            WDTPZ=0.
            WDTP(I)=WDTPZP
          ELSEIF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
            WDTP(I)=(VPI**2+API**2)*VINT(116)*WDTPZP
          ELSEIF(MINT(61).EQ.2) THEN
            FGGF=0.
            FGZF=0.
            FGZPF=0.
            FZZF=0.
            FZZPF=0.
            FZPZPF=WDTPZP
          ENDIF
          WID2=WIDS(24,1)
        ELSEIF(I.EQ.18) THEN
C...Z'0 -> H+ + H-
          CZC=2.*(1.-2.*XW)
          BE34C=(1.-4.*RM1)*SQRT(MAX(0.,1.-4.*RM1))
          IF(ICASE.EQ.1) THEN
            WDTPZ=0.25*PARU(142)**2*CZC**2*BE34C
            WDTP(I)=0.25*PARU(143)**2*CZC**2*BE34C
          ELSEIF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
            WDTP(I)=0.25*(EI**2*VINT(111)+PARU(142)*EI*VI*VINT(112)*
     &      CZC+PARU(143)*EI*VPI*VINT(113)*CZC+PARU(142)**2*
     &      (VI**2+AI**2)*VINT(114)*CZC**2+PARU(142)*PARU(143)*
     &      (VI*VPI+AI*API)*VINT(115)*CZC**2+PARU(143)**2*
     &      (VPI**2+API**2)*VINT(116)*CZC**2)*BE34C
          ELSEIF(MINT(61).EQ.2) THEN
            FGGF=0.25*BE34C
            FGZF=0.25*PARU(142)*CZC*BE34C
            FGZPF=0.25*PARU(143)*CZC*BE34C
            FZZF=0.25*PARU(142)**2*CZC**2*BE34C
            FZZPF=0.25*PARU(142)*PARU(143)*CZC**2*BE34C
            FZPZPF=0.25*PARU(143)**2*CZC**2*BE34C
          ENDIF
          WID2=WIDS(37,1)
        ELSEIF(I.EQ.19) THEN
C...Z'0 -> Z0 + gamma.
        ELSEIF(I.EQ.20) THEN
C...Z'0 -> Z0 + H0
          FLAM=SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))
          WDTPZP=PARU(145)**2*4.*ABS(1.-2.*XW)*(3.*RM1+0.25*FLAM**2)*
     &    FLAM
          IF(ICASE.EQ.1) THEN
            WDTPZ=0.
            WDTP(I)=WDTPZP
          ELSEIF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
            WDTP(I)=(VPI**2+API**2)*VINT(116)*WDTPZP
          ELSEIF(MINT(61).EQ.2) THEN
            FGGF=0.
            FGZF=0.
            FGZPF=0.
            FZZF=0.
            FZZPF=0.
            FZPZPF=WDTPZP
          ENDIF
          WID2=WIDS(23,2)*WIDS(25,2)
        ELSEIF(I.EQ.21.OR.I.EQ.22) THEN
C...Z' -> H0 + A0 or H'0 + A0.
          BE34C=SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))**3
          IF(I.EQ.21) THEN
            CZAH=PARU(186)
            CZPAH=PARU(188)
          ELSE
            CZAH=PARU(187)
            CZPAH=PARU(189)
          ENDIF
          IF(ICASE.EQ.1) THEN
            WDTPZ=CZAH**2*BE34C
            WDTP(I)=CZPAH**2*BE34C
          ELSEIF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
            WDTP(I)=(CZAH**2*(VI**2+AI**2)*VINT(114)+CZAH*CZPAH*
     &      (VI*VPI+AI*API)*VINT(115)+CZPAH**2*(VPI**2+API**2)*
     &      VINT(116))*BE34C
          ELSEIF(MINT(61).EQ.2) THEN
            FGGF=0.
            FGZF=0.
            FGZPF=0.
            FZZF=CZAH**2*BE34C
            FZZPF=CZAH*CZPAH*BE34C
            FZPZPF=CZPAH**2*BE34C
          ENDIF
          IF(I.EQ.21) WID2=WIDS(25,2)*WIDS(36,2)
          IF(I.EQ.22) WID2=WIDS(35,2)*WIDS(36,2)
        ENDIF
        IF(ICASE.EQ.1) THEN
          VINT(117)=VINT(117)+WDTPZ
          WDTP(0)=WDTP(0)+WDTP(I)
        ENDIF
        IF(MDME(IDC,1).GT.0) THEN
          IF((ICASE.EQ.1.AND.MINT(61).NE.1).OR.
     &    (ICASE.EQ.2.AND.MINT(61).EQ.1)) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
          IF(MINT(61).EQ.2.AND.ICASE.EQ.2) THEN
            IF(MSTP(44).EQ.1.OR.MSTP(44).EQ.4.OR.MSTP(44).EQ.5.OR.
     &      MSTP(44).EQ.7) VINT(111)=VINT(111)+FGGF*WID2
            IF(MSTP(44).EQ.4.OR.MSTP(44).EQ.7) VINT(112)=VINT(112)+
     &      FGZF*WID2
            IF(MSTP(44).EQ.5.OR.MSTP(44).EQ.7) VINT(113)=VINT(113)+
     &      FGZPF*WID2
            IF(MSTP(44).EQ.2.OR.MSTP(44).EQ.4.OR.MSTP(44).EQ.6.OR.
     &      MSTP(44).EQ.7) VINT(114)=VINT(114)+FZZF*WID2
            IF(MSTP(44).EQ.6.OR.MSTP(44).EQ.7) VINT(115)=VINT(115)+
     &      FZZPF*WID2
            IF(MSTP(44).EQ.3.OR.MSTP(44).EQ.5.OR.MSTP(44).EQ.6.OR.
     &      MSTP(44).EQ.7) VINT(116)=VINT(116)+FZPZPF*WID2
          ENDIF
        ENDIF
  210   CONTINUE
        IF(MINT(61).GE.1) ICASE=3-ICASE
        IF(ICASE.EQ.2) GOTO 200
 
      ELSEIF(KFLA.EQ.34) THEN
C...W'+/-:
        DO 220 I=1,MDCY(34,3)
        IDC=I+MDCY(34,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 220
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 220
        IF(I.LE.20) THEN
          IF(I.LE.16) THEN
C...W'+/- -> q + q~'
            FCOF=3.*RADC*(PARU(131)**2+PARU(132)**2)*
     &      VCKM((I-1)/4+1,MOD(I-1,4)+1)
          ELSEIF(I.LE.20) THEN
C...W'+/- -> l+/- + nu
            FCOF=PARU(133)**2+PARU(134)**2
          ENDIF
          WDTP(I)=FCOF*0.5*(2.-RM1-RM2-(RM1-RM2)**2)*
     &    SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))
          WID2=1.
        ELSEIF(I.EQ.21) THEN
C...W'+/- -> W+/- + Z0
          WDTP(I)=PARU(135)**2*0.5*(1.-XW)*(RM1/RM2)*
     &    SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))**3*
     &    (1.+10.*RM1+10.*RM2+RM1**2+RM2**2+10.*RM1*RM2)
          IF(KFLR.GT.0) WID2=WIDS(24,2)*WIDS(23,2)
          IF(KFLR.LT.0) WID2=WIDS(24,3)*WIDS(23,2)
        ELSEIF(I.EQ.23) THEN
C...W'+/- -> W+/- + H0
          FLAM=SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))
          WDTP(I)=PARU(146)**2*2.*(3.*RM1+0.25*FLAM**2)*FLAM
          IF(KFLR.GT.0) WID2=WIDS(24,2)*WIDS(25,2)
          IF(KFLR.LT.0) WID2=WIDS(24,3)*WIDS(25,2)
        ENDIF
        WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
          WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
          WDTE(I,0)=WDTE(I,MDME(IDC,1))
          WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
        ENDIF
  220   CONTINUE
 
      ELSEIF(KFLA.EQ.37) THEN
C...H+/-:
        DO 230 I=1,MDCY(37,3)
        IDC=I+MDCY(37,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 230
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 230
        IF(I.LE.4) THEN
C...H+/- -> q + q~'
          WDTP(I)=3.*RADC*((RM1*PARU(141)**2+RM2/PARU(141)**2)*
     &    (1.-RM1-RM2)-4.*RM1*RM2)*
     &    SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))
          WID2=1.
        ELSEIF(I.LE.8) THEN
C...H+/- -> l+/- + nu
          WDTP(I)=((RM1*PARU(141)**2+RM2/PARU(141)**2)*(1.-RM1-RM2)-
     &    4.*RM1*RM2)*SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))
          WID2=1.
        ELSEIF(I.EQ.9) THEN
C...H+/- -> W+/- + H0.
          WDTP(I)=PARU(195)**2*0.5*SQRT(MAX(0.,(1.-RM1-RM2)**2-
     &    4.*RM1*RM2))**3
          IF(KFLR.GT.0) WID2=WIDS(24,2)*WIDS(25,2)
          IF(KFLR.LT.0) WID2=WIDS(24,3)*WIDS(25,2)
        ENDIF
        WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
          WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
          WDTE(I,0)=WDTE(I,MDME(IDC,1))
          WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
        ENDIF
  230   CONTINUE
 
      ELSEIF(KFLA.EQ.39) THEN
C...LQ (leptoquark).
        DO 240 I=1,MDCY(39,3)
        IDC=I+MDCY(39,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 240
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 240
        WDTP(I)=PARU(151)*SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))**3
        WID2=1.
        WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
          WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
          WDTE(I,0)=WDTE(I,MDME(IDC,1))
          WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
        ENDIF
  240   CONTINUE
 
      ELSEIF(KFLA.EQ.40) THEN
C...R:
        DO 250 I=1,MDCY(40,3)
        IDC=I+MDCY(40,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 250
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 250
        IF(I.LE.6) THEN
C...R -> q + q~'
          FCOF=3.*RADC
        ELSEIF(I.LE.9) THEN
C...R -> l+ + l'-
          FCOF=1.
        ENDIF
        WDTP(I)=FCOF*(2.-RM1-RM2-(RM1-RM2)**2)*
     &  SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))
        WID2=1.
        WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
          WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
          WDTE(I,0)=WDTE(I,MDME(IDC,1))
          WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
        ENDIF
  250   CONTINUE
 
      ENDIF
      MINT(61)=0
      MINT(62)=0
 
      RETURN
      END
