*
* $Id: pyinre.F,v 1.1.1.1 1996/01/11 14:05:27 mclareni Exp $
*
* $Log: pyinre.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:27  mclareni
* Fritiof
*
*
C*********************************************************************
 
      SUBROUTINE PYINRE
 
C...Calculates full and effective widths of gauge bosons, stores
C...masses and widths, rescales coefficients to be used for
C...resonance production generation.
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      COMMON/LUDAT4/CHAF(500)
      CHARACTER CHAF*8
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYINT1/MINT(400),VINT(400)
      COMMON/PYINT2/ISET(200),KFPR(200,2),COEF(200,20),ICOL(40,4,2)
      COMMON/PYINT4/WIDP(21:40,0:40),WIDE(21:40,0:40),WIDS(21:40,3)
      COMMON/PYINT6/PROC(0:200)
      CHARACTER PROC*28
      SAVE /LUDAT1/,/LUDAT2/,/LUDAT3/,/LUDAT4/
      SAVE /PYSUBS/,/PYPARS/,/PYINT1/,/PYINT2/,/PYINT4/,/PYINT6/
      DIMENSION WDTP(0:40),WDTE(0:40,0:5)
 
C...Born level couplings in MSSM Higgs doublet sector.
      IF(MSTP(4).EQ.2) THEN
        TANBE=PARU(141)
        RATBE=((1.-TANBE**2)/(1.+TANBE**2))**2
        SQMZ=PMAS(23,1)**2
        SQMW=PMAS(24,1)**2
        SQMH=PMAS(25,1)**2
        SQMA=SQMH*(SQMZ-SQMH)/(SQMZ*RATBE-SQMH)
        SQMHP=0.5*(SQMA+SQMZ+SQRT((SQMA+SQMZ)**2-4.*SQMA*SQMZ*RATBE))
        SQMHC=SQMA+SQMW
        IF(MIN(SQMA,SQMHP,SQMHC).LE.0.) THEN
          WRITE(MSTU(11),5000)
          STOP
        ENDIF
        PMAS(35,1)=SQRT(SQMHP)
        PMAS(36,1)=SQRT(SQMA)
        PMAS(37,1)=SQRT(SQMHC)
        ALSU=0.5*ATAN(2.*TANBE*(SQMA+SQMZ)/((1.-TANBE**2)*
     &  (SQMA-SQMZ)))
        BESU=ATAN(TANBE)
        PARU(161)=-SIN(ALSU)/COS(BESU)
        PARU(162)=COS(ALSU)/SIN(BESU)
        PARU(163)=PARU(161)
        PARU(164)=SIN(BESU-ALSU)
        PARU(165)=PARU(164)
        PARU(171)=COS(ALSU)/COS(BESU)
        PARU(172)=SIN(ALSU)/SIN(BESU)
        PARU(173)=PARU(171)
        PARU(174)=COS(BESU-ALSU)
        PARU(175)=PARU(174)
        PARU(176)=COS(2.*ALSU)*COS(BESU+ALSU)-2.*SIN(2.*ALSU)*
     &  SIN(BESU+ALSU)
        PARU(177)=COS(2.*BESU)*COS(BESU+ALSU)
        PARU(181)=-TANBE
        PARU(182)=-1./TANBE
        PARU(183)=PARU(181)
        PARU(184)=0.
        PARU(185)=PARU(184)
        PARU(186)=COS(BESU-ALSU)
        PARU(187)=SIN(BESU-ALSU)
        PARU(195)=COS(BESU-ALSU)
      ENDIF
 
C...Reset full and effective widths of gauge bosons.
      XW=PARU(102)
      DO 100 I=21,40
      DO 100 J=0,40
      WIDP(I,J)=0.
  100 WIDE(I,J)=0.
 
C...Loop over possible resonances.
      DO 130 I=1,10
      IF(I.LE.3) KC=I+22
      IF(I.EQ.4) KC=37
      IF(I.EQ.5) KC=36
      IF(I.EQ.6) KC=35
      IF(I.EQ.7) KC=32
      IF(I.EQ.8) KC=34
      IF(I.GE.9) KC=I+30
 
C...Check that no fourth generation channels on by mistake.
      IF(MSTP(1).LE.3) THEN
        DO 110 J=1,MDCY(KC,3)
        IDC=J+MDCY(KC,2)-1
        KFA1=IABS(KFDP(IDC,1))
        KFA2=IABS(KFDP(IDC,2))
  110   IF(KFA1.EQ.7.OR.KFA1.EQ.8.OR.KFA1.EQ.17.OR.KFA1.EQ.18.OR.KFA2.
     &  EQ.7.OR.KFA2.EQ.8.OR.KFA2.EQ.17.OR.KFA2.EQ.18) MDME(IDC,1)=-1
      ENDIF
 
C...Find mass and evaluate width.
      PMR=PMAS(KC,1)
      IF(KC.EQ.25.OR.KC.EQ.35.OR.KC.EQ.36) MINT(62)=1
      CALL PYWIDT(KC,PMR**2,WDTP,WDTE)
 
C...Evaluate suppression factors due to non-simulated channels.
      IF(KCHG(KC,3).EQ.0) THEN
        WIDS(KC,1)=((WDTE(0,1)+WDTE(0,2))**2+
     &  2.*(WDTE(0,1)+WDTE(0,2))*(WDTE(0,4)+WDTE(0,5))+
     &  2.*WDTE(0,4)*WDTE(0,5))/WDTP(0)**2
        WIDS(KC,2)=(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))/WDTP(0)
        WIDS(KC,3)=0.
      ELSE
        WIDS(KC,1)=((WDTE(0,1)+WDTE(0,2))*(WDTE(0,1)+WDTE(0,3))+
     &  (WDTE(0,1)+WDTE(0,2)+WDTE(0,1)+WDTE(0,3))*(WDTE(0,4)+WDTE(0,5))+
     &  2.*WDTE(0,4)*WDTE(0,5))/WDTP(0)**2
        WIDS(KC,2)=(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))/WDTP(0)
        WIDS(KC,3)=(WDTE(0,1)+WDTE(0,3)+WDTE(0,4))/WDTP(0)
        IF(KC.EQ.24) THEN
          VINT(91)=((WDTE(0,1)+WDTE(0,2))**2+2.*(WDTE(0,1)+WDTE(0,2))*
     &    (WDTE(0,4)+WDTE(0,5))+2.*WDTE(0,4)*WDTE(0,5))/WDTP(0)**2
          VINT(92)=((WDTE(0,1)+WDTE(0,3))**2+2.*(WDTE(0,1)+WDTE(0,3))*
     &    (WDTE(0,4)+WDTE(0,5))+2.*WDTE(0,4)*WDTE(0,5))/WDTP(0)**2
        ENDIF
      ENDIF
 
C...Find factors to give widths in GeV.
      AEM=ULALEM(PMR**2)
      IF(KC.EQ.23) THEN
        FAC=AEM/(48.*XW*(1.-XW))*PMR
      ELSEIF(KC.EQ.24) THEN
        FAC=AEM/(24.*XW)*PMR
      ELSEIF(KC.EQ.25.OR.KC.EQ.35.OR.KC.EQ.36) THEN
        FAC=AEM/(8.*XW)*(PMR/PMAS(24,1))**2*PMR
      ELSEIF(KC.EQ.32) THEN
        FAC=AEM/(48.*XW*(1.-XW))*PMR
      ELSEIF(KC.EQ.34) THEN
        FAC=AEM/(24.*XW)*PMR
      ELSEIF(KC.EQ.37) THEN
        FAC=AEM/(8.*XW)*(PMR/PMAS(24,1))**2*PMR
      ELSEIF(KC.EQ.39) THEN
        FAC=AEM/4.*PMR
      ELSEIF(KC.EQ.40) THEN
        FAC=AEM/(12.*XW)*PMR
      ENDIF
 
C...Translate widths into GeV and save them.
      DO 120 J=0,40
      WIDP(KC,J)=FAC*WDTP(J)
  120 WIDE(KC,J)=FAC*WDTE(J,0)
 
C...Set resonance widths and branching ratios in JETSET;
C...also on/off switch for decays in PYTHIA/JETSET.
      PMAS(KC,2)=WIDP(KC,0)
      PMAS(KC,3)=MIN(0.9*PMAS(KC,1),10.*PMAS(KC,2))
      MDCY(KC,1)=MSTP(41)
      DO 130 J=1,MDCY(KC,3)
      IDC=J+MDCY(KC,2)-1
      BRAT(IDC)=WIDE(KC,J)/WIDE(KC,0)
  130 CONTINUE
 
C...Find heaviest new quark flavour allowed in processes 81-84.
      KFLQM=1
      DO 140 I=1,MIN(8,MDCY(21,3))
      IDC=I+MDCY(21,2)-1
      IF(MDME(IDC,1).LE.0) GOTO 140
      KFLQM=I
  140 CONTINUE
      MINT(55)=KFLQM
      KFPR(81,1)=KFLQM
      KFPR(81,2)=KFLQM
      KFPR(82,1)=KFLQM
      KFPR(82,2)=KFLQM
      KFPR(83,1)=KFLQM
      KFPR(84,1)=KFLQM
      KFPR(84,2)=KFLQM
 
C...Find heaviest new fermion flavour allowed in process 85.
      KFLFM=1
      DO 150 I=1,MIN(12,MDCY(22,3))
      IDC=I+MDCY(22,2)-1
      IF(MDME(IDC,1).LE.0) GOTO 150
      KFLFM=KFDP(IDC,1)
  150 CONTINUE
      MINT(56)=KFLFM
      KFPR(85,1)=KFLFM
      KFPR(85,2)=KFLFM
 
C...Flavours of leptoquark: redefine charge and name.
      KFLQQ=KFDP(MDCY(39,2),1)
      KFLQL=KFDP(MDCY(39,2),2)
      KCHG(39,1)=KCHG(IABS(KFLQQ),1)*ISIGN(1,KFLQQ)+
     &KCHG(IABS(KFLQL),1)*ISIGN(1,KFLQL)
      CHAF(39)(4:4)=CHAF(IABS(KFLQQ))(1:1)
      CHAF(39)(5:7)=CHAF(IABS(KFLQL))(1:3)
 
C...Special cases in treatment of gamma*/Z0: redefine process name.
      IF(MSTP(43).EQ.1) THEN
        PROC(1)='f + f~ -> gamma*'
      ELSEIF(MSTP(43).EQ.2) THEN
        PROC(1)='f + f~ -> Z0'
      ELSEIF(MSTP(43).EQ.3) THEN
        PROC(1)='f + f~ -> gamma*/Z0'
      ENDIF
 
C...Special cases in treatment of gamma*/Z0/Z'0: redefine process name.
      IF(MSTP(44).EQ.1) THEN
        PROC(141)='f + f~ -> gamma*'
      ELSEIF(MSTP(44).EQ.2) THEN
        PROC(141)='f + f~ -> Z0'
      ELSEIF(MSTP(44).EQ.3) THEN
        PROC(141)='f + f~ -> Z''0'
      ELSEIF(MSTP(44).EQ.4) THEN
        PROC(141)='f + f~ -> gamma*/Z0'
      ELSEIF(MSTP(44).EQ.5) THEN
        PROC(141)='f + f~ -> gamma*/Z''0'
      ELSEIF(MSTP(44).EQ.6) THEN
        PROC(141)='f + f~ -> Z0/Z''0'
      ELSEIF(MSTP(44).EQ.7) THEN
        PROC(141)='f + f~ -> gamma*/Z0/Z''0'
      ENDIF
 
C...Special cases in treatment of WW -> WW: redefine process name.
      IF(MSTP(45).EQ.1) THEN
        PROC(77)='W+ + W+ -> W+ + W+'
      ELSEIF(MSTP(45).EQ.2) THEN
        PROC(77)='W+ + W- -> W+ + W-'
      ELSEIF(MSTP(45).EQ.3) THEN
        PROC(77)='W+/- + W+/- -> W+/- + W+/-'
      ENDIF
 
C...Format for error information.
 5000 FORMAT(1X,'Error: unphysical input tan^2(beta) and m_H ',
     &'combination'/1X,'Execution stopped!')
 
      RETURN
      END
