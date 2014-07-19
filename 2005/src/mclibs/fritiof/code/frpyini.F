*
* $Id: frpyini.F,v 1.1.1.1 1996/01/11 14:05:22 mclareni Exp $
*
* $Log: frpyini.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:22  mclareni
* Fritiof
*
*
C***************************** END FRVALUE *****************************
 
C***********************************************************************
C..............Subroutines originally from PYTHIA.......................
C... modified slightly for FRITIOF to accomodate some changes in the way
C... meson-nucleon cross sections are handled.
C.......................................................................
 
      SUBROUTINE FRPYINI(FRAME,BEAM,TARGET,WIN)
 
C...This routine is identical to PYTHIA's PYINIT except the routine
C...for xsections PYXTOT is replaced.
 
C...Initializes the generation procedure; finds maxima of the
C...differential cross-sections to be used for weighting.
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      COMMON/LUDAT4/CHAF(500)
      CHARACTER CHAF*8
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYINT1/MINT(400),VINT(400)
      COMMON/PYINT2/ISET(200),KFPR(200,2),COEF(200,20),ICOL(40,4,2)
      COMMON/PYINT5/NGEN(0:200,3),XSEC(0:200,3)
      SAVE /LUDAT1/,/LUDAT2/,/LUDAT3/,/LUDAT4/
      SAVE /PYSUBS/,/PYPARS/,/PYINT1/,/PYINT2/,/PYINT5/
      DIMENSION ALAMIN(20),NFIN(20)
      CHARACTER*(*) FRAME,BEAM,TARGET
      CHARACTER CHFRAM*8,CHBEAM*8,CHTARG*8,CHMO(12)*3,CHLH(2)*6
      DATA ALAMIN/0.20,0.29,0.20,0.40,0.187,0.212,0.191,0.155,
     &0.22,0.16,0.16,0.26,0.36,7*0.2/,NFIN/20*4/
      DATA CHMO/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep',
     &'Oct','Nov','Dec'/, CHLH/'lepton','hadron'/
 
CC    CALL PYDATA
 
C...Reset MINT and VINT arrays. Write headers.
      DO 100 J=1,400
      MINT(J)=0
  100 VINT(J)=0.
      IF(MSTP(127).GE.1) WRITE(MSTU(11),5000) MSTP(181),MSTP(182),
     &MSTP(185),CHMO(MSTP(184)),MSTP(183)
      MSTP(127)=0
      IF(MSTU(12).GE.1) CALL LULIST(0)
      IF(MSTP(122).GE.1) WRITE(MSTU(11),5100)
 
C...Identify beam and target particles and initialize kinematics.
      CHFRAM=FRAME//' '
      CHBEAM=BEAM//' '
      CHTARG=TARGET//' '
      CALL PYINKI(CHFRAM,CHBEAM,CHTARG,WIN)
      IF(MINT(65).EQ.1) GOTO 160
 
C...Select partonic subprocesses to be included in the simulation.
      IF(MSEL.NE.0) THEN
        DO 110 I=1,200
  110   MSUB(I)=0
      ENDIF
      IF(MINT(43).EQ.1.AND.(MSEL.EQ.1.OR.MSEL.EQ.2)) THEN
C...Lepton+lepton -> gamma/Z0 or W.
        IF(MINT(11)+MINT(12).EQ.0) MSUB(1)=1
        IF(MINT(11)+MINT(12).NE.0) MSUB(2)=1
      ELSEIF(MINT(43).LE.3.AND.(MSEL.EQ.1.OR.MSEL.EQ.2)) THEN
C...Lepton+hadron: deep inelastic scattering.
        MSUB(11)=1
      ELSEIF(MSEL.EQ.1) THEN
C...High-pT QCD processes:
        MSUB(11)=1
        MSUB(12)=1
        MSUB(13)=1
        MSUB(28)=1
        MSUB(53)=1
        MSUB(68)=1
        IF(MSTP(82).LE.1.AND.CKIN(3).LT.PARP(81)) MSUB(95)=1
        IF(MSTP(82).GE.2.AND.CKIN(3).LT.PARP(82)) MSUB(95)=1
      ELSEIF(MSEL.EQ.2) THEN
C...All QCD processes:
        MSUB(11)=1
        MSUB(12)=1
        MSUB(13)=1
        MSUB(28)=1
        MSUB(53)=1
        MSUB(68)=1
        MSUB(91)=1
        MSUB(92)=1
        MSUB(93)=1
        MSUB(95)=1
      ELSEIF(MSEL.GE.4.AND.MSEL.LE.8) THEN
C...Heavy quark production.
        MSUB(81)=1
        MSUB(82)=1
        MSUB(84)=1
        DO 120 J=1,MIN(8,MDCY(21,3))
  120   MDME(MDCY(21,2)+J-1,1)=0
        MDME(MDCY(21,2)+MSEL-1,1)=1
        MSUB(85)=1
        DO 130 J=1,MIN(8,MDCY(22,3))
  130   MDME(MDCY(22,2)+J-1,1)=0
        MDME(MDCY(22,2)+MSEL-1,1)=1
      ELSEIF(MSEL.EQ.10) THEN
C...Prompt photon production:
        MSUB(14)=1
        MSUB(18)=1
        MSUB(29)=1
      ELSEIF(MSEL.EQ.11) THEN
C...Z0/gamma* production:
        MSUB(1)=1
      ELSEIF(MSEL.EQ.12) THEN
C...W+/- production:
        MSUB(2)=1
      ELSEIF(MSEL.EQ.13) THEN
C...Z0 + jet:
        MSUB(15)=1
        MSUB(30)=1
      ELSEIF(MSEL.EQ.14) THEN
C...W+/- + jet:
        MSUB(16)=1
        MSUB(31)=1
      ELSEIF(MSEL.EQ.15) THEN
C...Z0 & W+/- pair production:
        MSUB(19)=1
        MSUB(20)=1
        MSUB(22)=1
        MSUB(23)=1
        MSUB(25)=1
      ELSEIF(MSEL.EQ.16) THEN
C...H0 production:
        MSUB(3)=1
        MSUB(102)=1
        MSUB(103)=1
        MSUB(123)=1
        MSUB(124)=1
      ELSEIF(MSEL.EQ.17) THEN
C...H0 & Z0 or W+/- pair production:
        MSUB(24)=1
        MSUB(26)=1
      ELSEIF(MSEL.EQ.18) THEN
C...H0 production; interesting processes in e+e-.
        MSUB(24)=1
        MSUB(103)=1
        MSUB(123)=1
        MSUB(124)=1
      ELSEIF(MSEL.EQ.19) THEN
C...H0, H'0 and A0 production; interesting processes in e+e-.
        MSUB(24)=1
        MSUB(103)=1
        MSUB(123)=1
        MSUB(124)=1
        MSUB(153)=1
        MSUB(171)=1
        MSUB(173)=1
        MSUB(174)=1
        MSUB(158)=1
        MSUB(176)=1
        MSUB(178)=1
        MSUB(179)=1
      ELSEIF(MSEL.EQ.21) THEN
C...Z'0 production:
        MSUB(141)=1
      ELSEIF(MSEL.EQ.22) THEN
C...W'+/- production:
        MSUB(142)=1
      ELSEIF(MSEL.EQ.23) THEN
C...H+/- production:
        MSUB(143)=1
      ELSEIF(MSEL.EQ.24) THEN
C...R production:
        MSUB(144)=1
      ELSEIF(MSEL.EQ.25) THEN
C...LQ (leptoquark) production.
        MSUB(145)=1
        MSUB(162)=1
        MSUB(163)=1
        MSUB(164)=1
      ELSEIF(MSEL.GE.35.AND.MSEL.LE.38) THEN
C...Production of one heavy quark (W exchange):
        MSUB(83)=1
        DO 140 J=1,MIN(8,MDCY(21,3))
  140   MDME(MDCY(21,2)+J-1,1)=0
        MDME(MDCY(21,2)+MSEL-31,1)=1
      ENDIF
 
C...Count number of subprocesses on.
      MINT(48)=0
      DO 150 ISUB=1,200
      IF(MINT(44).LT.4.AND.ISUB.GE.91.AND.ISUB.LE.96.AND.
     &MSUB(ISUB).EQ.1) THEN
        WRITE(MSTU(11),5200) ISUB,CHLH(MINT(41)),CHLH(MINT(42))
        STOP
      ELSEIF(MSUB(ISUB).EQ.1.AND.ISET(ISUB).EQ.-1) THEN
        WRITE(MSTU(11),5300) ISUB
        STOP
      ELSEIF(MSUB(ISUB).EQ.1.AND.ISET(ISUB).LE.-2) THEN
        WRITE(MSTU(11),5400) ISUB
        STOP
      ELSEIF(MSUB(ISUB).EQ.1) THEN
        MINT(48)=MINT(48)+1
      ENDIF
  150 CONTINUE
      IF(MINT(48).EQ.0) THEN
        WRITE(MSTU(11),5500)
        STOP
      ENDIF
      MINT(49)=MINT(48)-MSUB(91)-MSUB(92)-MSUB(93)-MSUB(94)
 
C...Maximum 4 generations; set maximum number of allowed flavours.
  160 MSTP(1)=MIN(4,MSTP(1))
      MSTU(114)=MIN(MSTU(114),2*MSTP(1))
      MSTP(54)=MIN(MSTP(54),2*MSTP(1))
 
C...Sum up Cabibbo-Kobayashi-Maskawa factors for each quark/lepton.
      DO 180 I=-20,20
      VINT(180+I)=0.
      IA=IABS(I)
      IF(IA.GE.1.AND.IA.LE.2*MSTP(1)) THEN
        DO 170 J=1,MSTP(1)
        IB=2*J-1+MOD(IA,2)
        IPM=(5-ISIGN(1,I))/2
        IDC=J+MDCY(IA,2)+2
  170   IF(MDME(IDC,1).EQ.1.OR.MDME(IDC,1).EQ.IPM) VINT(180+I)=
     &  VINT(180+I)+VCKM((IA+1)/2,(IB+1)/2)
      ELSEIF(IA.GE.11.AND.IA.LE.10+2*MSTP(1)) THEN
        VINT(180+I)=1.
      ENDIF
  180 CONTINUE
 
C...Choose Lambda value to use in alpha-strong.
      MSTU(111)=MSTP(2)
      IF(MSTP(3).GE.1) THEN
        ALAM=PARP(1)
        IF(MSTP(51).GE.1.AND.MSTP(51).LE.13) ALAM=ALAMIN(MSTP(51))
        PARP(1)=ALAM
        PARP(61)=ALAM
        PARU(112)=ALAM
        PARJ(81)=ALAM
        IF(MSTP(51).GE.1.AND.MSTP(51).LE.13) MSTU(112)=NFIN(MSTP(51))
      ENDIF
 
C...Initialize widths and partial widths for resonances.
      CALL PYINRE
      IF(MINT(65).EQ.1) GOTO 200
 
C...Reset variables for cross-section calculation.
      DO 190 I=0,200
      DO 190 J=1,3
      NGEN(I,J)=0
  190 XSEC(I,J)=0.
 
C...Find parametrized total cross-sections.
      IF(MINT(44).EQ.4) CALL FRPYXTO
 
C...Maxima of differential cross-sections.
      IF(MSTP(121).LE.1) CALL PYMAXI
 
C...Initialize possibility of pileup events.
      IF(MSTP(131).NE.0) CALL PYPILE(1)
 
C...Initialize multiple interactions with variable impact parameter.
      IF(MINT(44).EQ.4.AND.(MINT(49).NE.0.OR.MSTP(131).NE.0).AND.
     &MSTP(82).GE.2) CALL PYMULT(1)
  200 IF(MSTP(122).GE.1) WRITE(MSTU(11),5600)
 
C...Formats for initialization information.
 5000 FORMAT(///20X,'The Lund Monte Carlo - PYTHIA version ',I1,'.',I1/
     &20X,'**  Last date of change:  ',I2,1X,A3,1X,I4,'  **'/)
 5100 FORMAT('1',18('*'),1X,'PYINIT: initialization of PYTHIA ',
     &'routines',1X,17('*'))
 5200 FORMAT(1X,'Error: process number ',I3,' not meaningful for ',A6,
     &'-',A6,' interactions.'/1X,'Execution stopped!')
 5300 FORMAT(1X,'Error: requested subprocess',I4,' not implemented.'/
     &1X,'Execution stopped!')
 5400 FORMAT(1X,'Error: requested subprocess',I4,' not existing.'/
     &1X,'Execution stopped!')
 5500 FORMAT(1X,'Error: no subprocess switched on.'/
     &1X,'Execution stopped.')
 5600 FORMAT(/1X,22('*'),1X,'PYINIT: initialization completed',1X,
     &22('*'))
 
      RETURN
      END
