*
* $Id: pyrand.F,v 1.1.1.1 1996/01/11 14:05:27 mclareni Exp $
*
* $Log: pyrand.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:27  mclareni
* Fritiof
*
*
C*********************************************************************
 
      SUBROUTINE PYRAND
 
C...Generates quantities characterizing the high-pT scattering at the
C...parton level according to the matrix elements. Chooses incoming,
C...reacting partons, their momentum fractions and one of the possible
C...subprocesses.
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYINT1/MINT(400),VINT(400)
      COMMON/PYINT2/ISET(200),KFPR(200,2),COEF(200,20),ICOL(40,4,2)
      COMMON/PYINT3/XSFX(2,-40:40),ISIG(1000,3),SIGH(1000)
      COMMON/PYINT4/WIDP(21:40,0:40),WIDE(21:40,0:40),WIDS(21:40,3)
      COMMON/PYINT5/NGEN(0:200,3),XSEC(0:200,3)
      SAVE /LUDAT1/,/LUDAT2/
      SAVE /PYSUBS/,/PYPARS/,/PYINT1/,/PYINT2/,/PYINT3/,/PYINT4/,
     &/PYINT5/
      DOUBLE PRECISION SH,SQM1,SQM2,SQM3,SQM4,SQLA12,SQLA34,
     &THTER1,THTER2,THL,THU,THM,SQMMIN,SQMMAX,SQMI,SQMJ,SQMF,
     &SQUA,QUAR,B,C,EXPTH,THARG,RATLOG,TH
 
C...Initial values, specifically for (first) semihard interaction.
      MINT(17)=0
      MINT(18)=0
      VINT(143)=1.
      VINT(144)=1.
      IF(MINT(82).EQ.1) NGEN(0,2)=NGEN(0,2)+1
      IF(MSUB(95).EQ.1.OR.MINT(82).GE.2) CALL PYMULT(2)
      ISUB=0
  100 MINT(51)=0
 
C...Choice of process type - first event of pileup.
      IF(MINT(82).EQ.1.AND.(ISUB.LE.90.OR.ISUB.GT.96)) THEN
        RSUB=XSEC(0,1)*RLU(0)
        DO 110 I=1,200
        IF(MSUB(I).NE.1) GOTO 110
        ISUB=I
        RSUB=RSUB-XSEC(I,1)
        IF(RSUB.LE.0.) GOTO 120
  110   CONTINUE
  120   IF(ISUB.EQ.95) ISUB=96
 
C...Choice of inclusive process type - pileup events.
      ELSEIF(MINT(82).GE.2.AND.ISUB.EQ.0) THEN
        RSUB=VINT(131)*RLU(0)
        ISUB=96
        IF(RSUB.GT.VINT(106)) ISUB=93
        IF(RSUB.GT.VINT(106)+VINT(104)) ISUB=92
        IF(RSUB.GT.VINT(106)+VINT(104)+VINT(103)) ISUB=91
      ENDIF
      IF(MINT(82).EQ.1) NGEN(0,1)=NGEN(0,1)+1
      IF(MINT(82).EQ.1) NGEN(ISUB,1)=NGEN(ISUB,1)+1
      MINT(1)=ISUB
      ISTSB=ISET(ISUB)
 
C...Find resonances (explicit or implicit in cross-section).
      MINT(72)=0
      KFR1=0
      IF(ISTSB.EQ.1.OR.ISTSB.EQ.3.OR.ISTSB.EQ.5) THEN
        KFR1=KFPR(ISUB,1)
      ELSEIF(ISUB.EQ.24.OR.ISUB.EQ.25.OR.ISUB.EQ.171.OR.ISUB.EQ.176)
     &THEN
        KFR1=23
      ELSEIF(ISUB.EQ.23.OR.ISUB.EQ.26.OR.ISUB.EQ.172.OR.ISUB.EQ.177)
     &THEN
        KFR1=24
      ELSEIF(ISUB.GE.71.AND.ISUB.LE.77) THEN
        KFR1=25
        IF(MSTP(46).EQ.5) THEN
          KFR1=30
          PMAS(30,1)=PARP(45)
          PMAS(30,2)=PARP(45)**3/(96.*PARU(1)*246.**2)
        ENDIF
      ENDIF
      CKMX=CKIN(2)
      IF(CKMX.LE.0.) CKMX=VINT(1)
      IF(KFR1.NE.0) THEN
        IF(CKIN(1).GT.PMAS(KFR1,1)+20.*PMAS(KFR1,2).OR.
     &  CKMX.LT.PMAS(KFR1,1)-20.*PMAS(KFR1,2)) KFR1=0
      ENDIF
      IF(KFR1.NE.0) THEN
        TAUR1=PMAS(KFR1,1)**2/VINT(2)
        GAMR1=PMAS(KFR1,1)*PMAS(KFR1,2)/VINT(2)
        MINT(72)=1
        MINT(73)=KFR1
        VINT(73)=TAUR1
        VINT(74)=GAMR1
      ENDIF
      IF(ISUB.EQ.141) THEN
        KFR2=23
        TAUR2=PMAS(KFR2,1)**2/VINT(2)
        GAMR2=PMAS(KFR2,1)*PMAS(KFR2,2)/VINT(2)
        IF(CKIN(1).GT.PMAS(KFR2,1)+20.*PMAS(KFR2,2).OR.
     &  CKMX.LT.PMAS(KFR2,1)-20.*PMAS(KFR2,2)) KFR2=0
        IF(KFR2.NE.0.AND.KFR1.NE.0) THEN
          MINT(72)=2
          MINT(74)=KFR2
          VINT(75)=TAUR2
          VINT(76)=GAMR2
        ELSEIF(KFR2.NE.0) THEN
          KFR1=KFR2
          TAUR1=TAUR2
          GAMR1=GAMR2
          MINT(72)=1
          MINT(73)=KFR1
          VINT(73)=TAUR1
          VINT(74)=GAMR1
        ENDIF
      ENDIF
 
C...Find product masses and minimum pT of process,
C...optionally with broadening according to a truncated Breit-Wigner.
      VINT(63)=0.
      VINT(64)=0.
      MINT(71)=0
      VINT(71)=CKIN(3)
      IF(MINT(82).GE.2) VINT(71)=0.
      VINT(80)=1.
      IF(ISTSB.EQ.2.OR.ISTSB.EQ.4) THEN
        NBW=0
        DO 130 I=1,2
        IF(KFPR(ISUB,I).EQ.0) THEN
        ELSEIF(MSTP(42).LE.0.OR.PMAS(KFPR(ISUB,I),2).LT.PARP(41)) THEN
          VINT(62+I)=PMAS(KFPR(ISUB,I),1)**2
        ELSE
          NBW=NBW+1
        ENDIF
  130   CONTINUE
        IF(NBW.GE.1) THEN
          CALL PYOFSH(4,0,KFPR(ISUB,1),KFPR(ISUB,2),0.,PQM3,PQM4)
          IF(MINT(51).EQ.1) GOTO 100
          VINT(63)=PQM3**2
          VINT(64)=PQM4**2
        ENDIF
        IF(MIN(VINT(63),VINT(64)).LT.CKIN(6)**2) MINT(71)=1
        IF(MINT(71).EQ.1) VINT(71)=MAX(CKIN(3),CKIN(5))
      ELSEIF(ISTSB.EQ.6) THEN
        CALL PYOFSH(6,0,KFPR(ISUB,1),KFPR(ISUB,2),0.,PQM3,PQM4)
        IF(MINT(51).EQ.1) GOTO 100
        VINT(63)=PQM3**2
        VINT(64)=PQM4**2
      ENDIF
 
C...Prepare for additional variable choices in 2 -> 3.
      IF(ISTSB.EQ.5) THEN
        VINT(201)=0.
        VINT(206)=0.
        VINT(204)=PMAS(23,1)
        IF(ISUB.EQ.124) VINT(204)=PMAS(24,1)
        VINT(209)=VINT(204)
      ENDIF
 
      IF(ISTSB.EQ.0) THEN
C...Double or single diffractive, or elastic scattering:
C...choose m^2 according to 1/m^2 (diffractive), constant (elastic)
        IS=INT(1.5+RLU(0))
        VINT(63)=VINT(3)**2
        VINT(64)=VINT(4)**2
        IF(ISUB.EQ.92.OR.ISUB.EQ.93) VINT(62+IS)=PARP(111)**2
        IF(ISUB.EQ.93) VINT(65-IS)=PARP(111)**2
        SH=VINT(2)
        SQM1=VINT(3)**2
        SQM2=VINT(4)**2
        SQM3=VINT(63)
        SQM4=VINT(64)
        SQLA12=(SH-SQM1-SQM2)**2-4D0*SQM1*SQM2
        SQLA34=(SH-SQM3-SQM4)**2-4D0*SQM3*SQM4
        THTER1=SQM1+SQM2+SQM3+SQM4-(SQM1-SQM2)*(SQM3-SQM4)/SH-SH
        THTER2=SQRT(MAX(0D0,SQLA12))*SQRT(MAX(0D0,SQLA34))/SH
        THL=0.5D0*(THTER1-THTER2)
        THU=0.5D0*(THTER1+THTER2)
        THM=MIN(MAX(THL,DBLE(PARP(101))),THU)
        JTMAX=0
        IF(ISUB.EQ.92.OR.ISUB.EQ.93) JTMAX=ISUB-91
        DO 140 JT=1,JTMAX
        MINT(13+3*JT-IS*(2*JT-3))=1
        SQMMIN=VINT(59+3*JT-IS*(2*JT-3))
        SQMI=VINT(8-3*JT+IS*(2*JT-3))**2
        SQMJ=VINT(3*JT-1-IS*(2*JT-3))**2
        SQMF=VINT(68-3*JT+IS*(2*JT-3))
        SQUA=0.5D0*SH/SQMI*((1D0+(SQMI-SQMJ)/SH)*THM+SQMI-SQMF-
     &  SQMJ**2/SH+(SQMI+SQMJ)*SQMF/SH+(SQMI-SQMJ)**2/SH**2*SQMF)
        QUAR=SH/SQMI*(THM*(THM+SH-SQMI-SQMJ-SQMF*(1D0-(SQMI-SQMJ)/SH))+
     &  SQMI*SQMJ-SQMJ*SQMF*(1D0+(SQMI-SQMJ-SQMF)/SH))
        SQMMAX=SQUA+SQRT(MAX(0D0,SQUA**2-QUAR))
        IF(ABS(QUAR/SQUA**2).LT.1.D-06) SQMMAX=0.5D0*QUAR/SQUA
        SQMMAX=MIN(SQMMAX,(DBLE(VINT(1))-SQRT(SQMF))**2)
        VINT(59+3*JT-IS*(2*JT-3))=SQMMIN*(SQMMAX/SQMMIN)**RLU(0)
  140   CONTINUE
C...Choose t-hat according to exp(B*t-hat+C*t-hat^2).
        SQM3=VINT(63)
        SQM4=VINT(64)
        SQLA34=(SH-SQM3-SQM4)**2-4D0*SQM3*SQM4
        THTER1=SQM1+SQM2+SQM3+SQM4-(SQM1-SQM2)*(SQM3-SQM4)/SH-SH
        THTER2=SQRT(MAX(0D0,SQLA12))*SQRT(MAX(0D0,SQLA34))/SH
        THL=0.5D0*(THTER1-THTER2)
        THU=0.5D0*(THTER1+THTER2)
        B=VINT(121)
        C=VINT(122)
        IF(ISUB.EQ.92.OR.ISUB.EQ.93) THEN
          B=0.5D0*B
          C=0.5D0*C
        ENDIF
        THM=MIN(MAX(THL,DBLE(PARP(101))),THU)
        EXPTH=0D0
        THARG=B*(THM-THU)
        IF(THARG.GT.-20D0) EXPTH=EXP(THARG)
  150   TH=THU+LOG(EXPTH+(1D0-EXPTH)*DBLE(RLU(0)))/B
        TH=MAX(THM,MIN(THU,TH))
        RATLOG=MIN((B+C*(TH+THM))*(TH-THM),(B+C*(TH+THU))*(TH-THU))
        IF(RATLOG.LT.LOG(RLU(0))) GOTO 150
        VINT(21)=1.
        VINT(22)=0.
        VINT(23)=MIN(1D0,MAX(-1D0,(2D0*TH-THTER1)/THTER2))
 
C...Note: in the following, by In is meant the integral over the
C...quantity multiplying coefficient cn.
C...Choose tau according to h1(tau)/tau, where
C...h1(tau) = c1 + I1/I2*c2*1/tau + I1/I3*c3*1/(tau+tau_R) +
C...I1/I4*c4*tau/((s*tau-m^2)^2+(m*Gamma)^2) +
C...I1/I5*c5*1/(tau+tau_R') +
C...I1/I6*c6*tau/((s*tau-m'^2)^2+(m'*Gamma')^2) +
C...I1/I7*c7*tau/(1.-tau), and
C...c1 + c2 + c3 + c4 + c5 + c6 + c7 = 1.
      ELSEIF(ISTSB.GE.1.AND.ISTSB.LE.6) THEN
        CALL PYKLIM(1)
        IF(MINT(51).NE.0) GOTO 100
        RTAU=RLU(0)
        MTAU=1
        IF(RTAU.GT.COEF(ISUB,1)) MTAU=2
        IF(RTAU.GT.COEF(ISUB,1)+COEF(ISUB,2)) MTAU=3
        IF(RTAU.GT.COEF(ISUB,1)+COEF(ISUB,2)+COEF(ISUB,3)) MTAU=4
        IF(RTAU.GT.COEF(ISUB,1)+COEF(ISUB,2)+COEF(ISUB,3)+COEF(ISUB,4))
     &  MTAU=5
        IF(RTAU.GT.COEF(ISUB,1)+COEF(ISUB,2)+COEF(ISUB,3)+COEF(ISUB,4)+
     &  COEF(ISUB,5)) MTAU=6
        IF(RTAU.GT.COEF(ISUB,1)+COEF(ISUB,2)+COEF(ISUB,3)+COEF(ISUB,4)+
     &  COEF(ISUB,5)+COEF(ISUB,6)) MTAU=7
        CALL PYKMAP(1,MTAU,RLU(0))
 
C...2 -> 3, 4 processes:
C...Choose tau' according to h4(tau,tau')/tau', where
C...h4(tau,tau') = c1 + I1/I2*c2*(1 - tau/tau')^3/tau' +
C...I1/I3*c3*1/(1 - tau'), and c1 + c2 + c3 = 1.
        IF(ISTSB.GE.3.AND.ISTSB.LE.5) THEN
          CALL PYKLIM(4)
          IF(MINT(51).NE.0) GOTO 100
          RTAUP=RLU(0)
          MTAUP=1
          IF(RTAUP.GT.COEF(ISUB,18)) MTAUP=2
          IF(RTAUP.GT.COEF(ISUB,18)+COEF(ISUB,19)) MTAUP=3
          CALL PYKMAP(4,MTAUP,RLU(0))
        ENDIF
 
C...Choose y* according to h2(y*), where
C...h2(y*) = I0/I1*c1*(y*-y*min) + I0/I2*c2*(y*max-y*) +
C...I0/I3*c3*1/cosh(y*) + I0/I4*c4*1/(1-exp(y*-y*max)) +
C...I0/I5*c5*1/(1-exp(-y*-y*min)), I0 = y*max-y*min,
C...and c1 + c2 + c3 + c4 + c5 = 1.
        CALL PYKLIM(2)
        IF(MINT(51).NE.0) GOTO 100
        RYST=RLU(0)
        MYST=1
        IF(RYST.GT.COEF(ISUB,8)) MYST=2
        IF(RYST.GT.COEF(ISUB,8)+COEF(ISUB,9)) MYST=3
        IF(RYST.GT.COEF(ISUB,8)+COEF(ISUB,9)+COEF(ISUB,10)) MYST=4
        IF(RYST.GT.COEF(ISUB,8)+COEF(ISUB,9)+COEF(ISUB,10)+
     &  COEF(ISUB,11)) MYST=5
        CALL PYKMAP(2,MYST,RLU(0))
 
C...2 -> 2 processes:
C...Choose cos(theta-hat) (cth) according to h3(cth), where
C...h3(cth) = c0 + I0/I1*c1*1/(A - cth) + I0/I2*c2*1/(A + cth) +
C...I0/I3*c3*1/(A - cth)^2 + I0/I4*c4*1/(A + cth)^2,
C...A = 1 + 2*(m3*m4/sh)^2 (= 1 for massless products),
C...and c0 + c1 + c2 + c3 + c4 = 1.
        CALL PYKLIM(3)
        IF(MINT(51).NE.0) GOTO 100
        IF(ISTSB.EQ.2.OR.ISTSB.EQ.4.OR.ISTSB.EQ.6) THEN
          RCTH=RLU(0)
          MCTH=1
          IF(RCTH.GT.COEF(ISUB,13)) MCTH=2
          IF(RCTH.GT.COEF(ISUB,13)+COEF(ISUB,14)) MCTH=3
          IF(RCTH.GT.COEF(ISUB,13)+COEF(ISUB,14)+COEF(ISUB,15)) MCTH=4
          IF(RCTH.GT.COEF(ISUB,13)+COEF(ISUB,14)+COEF(ISUB,15)+
     &    COEF(ISUB,16)) MCTH=5
          CALL PYKMAP(3,MCTH,RLU(0))
        ENDIF
 
C...2 -> 3 : select pT1, phi1, pT2, phi2, y3 for 3 outgoing.
        IF(ISTSB.EQ.5) THEN
          CALL PYKMAP(5,0,0.)
          IF(MINT(51).NE.0) GOTO 100
        ENDIF
 
C...Low-pT or multiple interactions (first semihard interaction).
      ELSEIF(ISTSB.EQ.9) THEN
        CALL PYMULT(3)
        ISUB=MINT(1)
      ENDIF
 
C...Choose azimuthal angle.
      VINT(24)=PARU(2)*RLU(0)
 
C...Check against user cuts on kinematics at parton level.
      MINT(51)=0
      IF(ISUB.LE.90.OR.ISUB.GT.100) CALL PYKLIM(0)
      IF(MINT(51).NE.0) GOTO 100
      IF(MINT(82).EQ.1.AND.MSTP(141).GE.1) THEN
        MCUT=0
        IF(MSUB(91)+MSUB(92)+MSUB(93)+MSUB(94)+MSUB(95).EQ.0)
     &  CALL PYKCUT(MCUT)
        IF(MCUT.NE.0) GOTO 100
      ENDIF
 
C...Calculate differential cross-section for different subprocesses.
      CALL PYSIGH(NCHN,SIGS)
 
C...Calculations for Monte Carlo estimate of all cross-sections.
      IF(MINT(82).EQ.1.AND.ISUB.LE.90.OR.ISUB.GE.96) THEN
        XSEC(ISUB,2)=XSEC(ISUB,2)+SIGS
      ELSEIF(MINT(82).EQ.1) THEN
        XSEC(ISUB,2)=XSEC(ISUB,2)+XSEC(ISUB,1)
      ENDIF
 
C...Multiple interactions: store results of cross-section calculation.
      IF(MINT(44).EQ.4.AND.MSTP(82).GE.3) THEN
        VINT(153)=SIGS
        CALL PYMULT(4)
      ENDIF
 
C...Weighting using estimate of maximum of differential cross-section.
C...Check that weight not negative!
      VIOL=SIGS/XSEC(ISUB,1)
      IF(MSTP(123).LE.0) THEN
        IF(VIOL.LT.-1E-3) THEN
          WRITE(MSTU(11),5000) VIOL,NGEN(0,3)+1
          WRITE(MSTU(11),5100) ISUB,VINT(21),VINT(22),VINT(23),VINT(26)
          STOP
        ENDIF
      ELSE
        IF(VIOL.LT.MIN(-1E-3,VINT(109))) THEN
          VINT(109)=VIOL
          WRITE(MSTU(11),5200) VIOL,NGEN(0,3)+1
          WRITE(MSTU(11),5100) ISUB,VINT(21),VINT(22),VINT(23),VINT(26)
        ENDIF
      ENDIF
      IF(VIOL.LT.RLU(0)) GOTO 100
 
C...Check for possible violation of estimated maximum of differential
C...cross-section used in weighting.
      IF(MSTP(123).LE.0) THEN
        IF(VIOL.GT.1.) THEN
          WRITE(MSTU(11),5300) VIOL,NGEN(0,3)+1
          WRITE(MSTU(11),5100) ISUB,VINT(21),VINT(22),VINT(23),VINT(26)
          STOP
        ENDIF
      ELSEIF(MSTP(123).EQ.1) THEN
        IF(VIOL.GT.VINT(108)) THEN
          VINT(108)=VIOL
          IF(VIOL.GT.1.) THEN
            WRITE(MSTU(11),5400) VIOL,NGEN(0,3)+1
            WRITE(MSTU(11),5100) ISUB,VINT(21),VINT(22),VINT(23),
     &      VINT(26)
          ENDIF
        ENDIF
      ELSEIF(VIOL.GT.VINT(108)) THEN
        VINT(108)=VIOL
        IF(VIOL.GT.1.) THEN
          XDIF=XSEC(ISUB,1)*(VIOL-1.)
          XSEC(ISUB,1)=XSEC(ISUB,1)+XDIF
          IF(MSUB(ISUB).EQ.1.AND.(ISUB.LE.90.OR.ISUB.GT.96))
     &    XSEC(0,1)=XSEC(0,1)+XDIF
          WRITE(MSTU(11),5400) VIOL,NGEN(0,3)+1
          WRITE(MSTU(11),5100) ISUB,VINT(21),VINT(22),VINT(23),VINT(26)
          IF(ISUB.LE.9) THEN
            WRITE(MSTU(11),5500) ISUB,XSEC(ISUB,1)
          ELSEIF(ISUB.LE.99) THEN
            WRITE(MSTU(11),5600) ISUB,XSEC(ISUB,1)
          ELSE
            WRITE(MSTU(11),5700) ISUB,XSEC(ISUB,1)
          ENDIF
          VINT(108)=1.
        ENDIF
      ENDIF
 
C...Multiple interactions: choose impact parameter.
      VINT(148)=1.
      IF(MINT(44).EQ.4.AND.(ISUB.LE.90.OR.ISUB.GE.96).AND.MSTP(82).GE.3)
     &THEN
        CALL PYMULT(5)
        IF(VINT(150).LT.RLU(0)) GOTO 100
      ENDIF
      IF(MINT(82).EQ.1.AND.MSUB(95).EQ.1) THEN
        IF(ISUB.LE.90.OR.ISUB.GE.95) NGEN(95,1)=NGEN(95,1)+1
        IF(ISUB.LE.90.OR.ISUB.GE.96) NGEN(96,2)=NGEN(96,2)+1
      ENDIF
      IF(ISUB.LE.90.OR.ISUB.GE.96) MINT(31)=MINT(31)+1
 
C...Choose flavour of reacting partons (and subprocess).
      RSIGS=SIGS*RLU(0)
      QT2=VINT(48)
      RQQBAR=PARP(87)*(1.-(QT2/(QT2+(PARP(88)*PARP(82))**2))**2)
      IF(ISUB.NE.95.AND.(ISUB.NE.96.OR.MSTP(82).LE.1.OR.
     &RLU(0).GT.RQQBAR)) THEN
        DO 160 ICHN=1,NCHN
        KFL1=ISIG(ICHN,1)
        KFL2=ISIG(ICHN,2)
        MINT(2)=ISIG(ICHN,3)
        RSIGS=RSIGS-SIGH(ICHN)
        IF(RSIGS.LE.0.) GOTO 170
  160   CONTINUE
 
C...Multiple interactions: choose qq~ preferentially at small pT.
      ELSEIF(ISUB.EQ.96) THEN
        CALL PYSPLI(MINT(11),21,KFL1,KFLDUM)
        CALL PYSPLI(MINT(12),21,KFL2,KFLDUM)
        MINT(1)=11
        MINT(2)=1
        IF(KFL1.EQ.KFL2.AND.RLU(0).LT.0.5) MINT(2)=2
 
C...Low-pT: choose string drawing configuration.
      ELSE
        KFL1=21
        KFL2=21
        RSIGS=6.*RLU(0)
        MINT(2)=1
        IF(RSIGS.GT.1.) MINT(2)=2
        IF(RSIGS.GT.2.) MINT(2)=3
      ENDIF
 
C...Reassign QCD process. Partons before initial state radiation.
  170 IF(MINT(2).GT.10) THEN
        MINT(1)=MINT(2)/10
        MINT(2)=MOD(MINT(2),10)
      ENDIF
      IF(MINT(82).EQ.1.AND.MSTP(111).GE.0) NGEN(MINT(1),2)=
     &NGEN(MINT(1),2)+1
      MINT(15)=KFL1
      MINT(16)=KFL2
      MINT(13)=MINT(15)
      MINT(14)=MINT(16)
      VINT(141)=VINT(41)
      VINT(142)=VINT(42)
      VINT(151)=0.
      VINT(152)=0.
 
C...Format statements for differential cross-section maximum violations.
 5000 FORMAT(1X,'Error: negative cross-section fraction',1P,E11.3,1X,
     &'in event',1X,I7,'.'/1X,'Execution stopped!')
 5100 FORMAT(1X,'ISUB = ',I3,'; Point of violation:'/1X,'tau =',1P,
     &E11.3,', y* =',E11.3,', cthe = ',0P,F11.7,', tau'' =',1P,E11.3)
 5200 FORMAT(1X,'Warning: negative cross-section fraction',1P,E11.3,1X,
     &'in event',1X,I7)
 5300 FORMAT(1X,'Error: maximum violated by',1P,E11.3,1X,
     &'in event',1X,I7,'.'/1X,'Execution stopped!')
 5400 FORMAT(1X,'Warning: maximum violated by',1P,E11.3,1X,
     &'in event',1X,I7)
 5500 FORMAT(1X,'XSEC(',I1,',1) increased to',1P,E11.3)
 5600 FORMAT(1X,'XSEC(',I2,',1) increased to',1P,E11.3)
 5700 FORMAT(1X,'XSEC(',I3,',1) increased to',1P,E11.3)
 
      RETURN
      END
