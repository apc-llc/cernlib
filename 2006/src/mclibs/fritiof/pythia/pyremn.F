*
* $Id: pyremn.F,v 1.1.1.1 1996/01/11 14:05:28 mclareni Exp $
*
* $Log: pyremn.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:28  mclareni
* Fritiof
*
*
C*********************************************************************
 
      SUBROUTINE PYREMN(IPU1,IPU2)
 
C...Adds on target remnants (one or two from each side) and
C...includes primordial kT for hadron beams.
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYINT1/MINT(400),VINT(400)
      SAVE /LUJETS/,/LUDAT1/,/LUDAT2/
      SAVE /PYPARS/,/PYINT1/
      DIMENSION KFLCH(2),KFLSP(2),CHI(2),PMS(0:6),IS(2),ISN(2),ROBO(5),
     &PSYS(0:2,5),PMIN(0:2)
 
C...Find event type and remaining energy.
      ISUB=MINT(1)
      NS=N
      IF(MINT(44).NE.4.OR.MSTP(81).LE.0) THEN
        VINT(143)=1.-VINT(141)
        VINT(144)=1.-VINT(142)
      ENDIF
 
C...Define initial partons.
  100 DO 130 JT=1,2
      I=MINT(83)+JT+2
      IF(JT.EQ.1) IPU=IPU1
      IF(JT.EQ.2) IPU=IPU2
      K(I,1)=21
      K(I,2)=K(IPU,2)
      K(I,3)=I-2
      PMS(JT)=0.
      IF(MINT(47).EQ.1) THEN
        DO 110 J=1,5
  110   P(I,J)=P(I-2,J)
      ELSEIF(ISUB.EQ.95) THEN
        K(I,2)=21
      ELSE
        P(I,5)=P(IPU,5)
 
C...No primordial kT, or chosen according to truncated Gaussian or
C...exponential.
  120   IF(MINT(40+JT).EQ.1.OR.MSTP(91).LE.0) THEN
          PT=0.
        ELSEIF(MSTP(91).EQ.1) THEN
          PT=PARP(91)*SQRT(-LOG(RLU(0)))
        ELSE
          RPT1=RLU(0)
          RPT2=RLU(0)
          PT=-PARP(92)*LOG(RPT1*RPT2)
        ENDIF
        IF(PT.GT.PARP(93)) GOTO 120
        PHI=PARU(2)*RLU(0)
        P(I,1)=PT*COS(PHI)
        P(I,2)=PT*SIN(PHI)
        PMS(JT)=P(I,5)**2+P(I,1)**2+P(I,2)**2
      ENDIF
  130 CONTINUE
      IF(MINT(47).EQ.1) RETURN
 
C...Kinematics construction for initial partons.
      I1=MINT(83)+3
      I2=MINT(83)+4
      IF(ISUB.EQ.95) THEN
        SHS=0.
        SHR=0.
      ELSE
        SHS=VINT(141)*VINT(142)*VINT(2)+(P(I1,1)+P(I2,1))**2+
     &  (P(I1,2)+P(I2,2))**2
        SHR=SQRT(MAX(0.,SHS))
        IF((SHS-PMS(1)-PMS(2))**2-4.*PMS(1)*PMS(2).LE.0.) GOTO 100
        P(I1,4)=0.5*(SHR+(PMS(1)-PMS(2))/SHR)
        P(I1,3)=SQRT(MAX(0.,P(I1,4)**2-PMS(1)))
        P(I2,4)=SHR-P(I1,4)
        P(I2,3)=-P(I1,3)
 
C...Transform partons to overall CM-frame.
        ROBO(3)=(P(I1,1)+P(I2,1))/SHR
        ROBO(4)=(P(I1,2)+P(I2,2))/SHR
        CALL LUDBRB(I1,I2,0.,0.,-DBLE(ROBO(3)),-DBLE(ROBO(4)),0D0)
        ROBO(2)=ULANGL(P(I1,1),P(I1,2))
        CALL LUDBRB(I1,I2,0.,-ROBO(2),0D0,0D0,0D0)
        ROBO(1)=ULANGL(P(I1,3),P(I1,1))
        CALL LUDBRB(I1,I2,-ROBO(1),0.,0D0,0D0,0D0)
        CALL LUDBRB(I1,MINT(52),ROBO(1),ROBO(2),DBLE(ROBO(3)),
     &  DBLE(ROBO(4)),0D0)
        ROBO(5)=MAX(-0.999999,MIN(0.999999,(VINT(141)-VINT(142))/
     &  (VINT(141)+VINT(142))))
        CALL LUDBRB(I1,MINT(52),0.,0.,0D0,0D0,DBLE(ROBO(5)))
      ENDIF
 
C...Check minimum invariant mass of remnant system(s).
      PSYS(0,4)=P(I1,4)+P(I2,4)+0.5*VINT(1)*(VINT(151)+VINT(152))
      PSYS(0,3)=P(I1,3)+P(I2,3)+0.5*VINT(1)*(VINT(151)-VINT(152))
      PMS(0)=MAX(0.,PSYS(0,4)**2-PSYS(0,3)**2)
      PMIN(0)=SQRT(PMS(0))
      DO 140 JT=1,2
      PSYS(JT,4)=0.5*VINT(1)*VINT(142+JT)
      PMIN(JT)=0.
      IF(MINT(40+JT).EQ.1.AND.MSTP(11).EQ.0) GOTO 140
      CALL PYSPLI(MINT(10+JT),MINT(12+JT),KFLCH(JT),KFLSP(JT))
      IF(KFLCH(JT).NE.0) PMIN(JT)=PMIN(JT)+ULMASS(KFLCH(JT))
      IF(KFLSP(JT).NE.0) PMIN(JT)=PMIN(JT)+ULMASS(KFLSP(JT))
      IF(KFLCH(JT)*KFLSP(JT).NE.0) PMIN(JT)=PMIN(JT)+0.5*PARP(111)
      PMIN(JT)=SQRT(PMIN(JT)**2+P(MINT(83)+JT+2,1)**2+
     &P(MINT(83)+JT+2,2)**2)
  140 CONTINUE
      IF(PMIN(0)+PMIN(1)+PMIN(2).GT.VINT(1).OR.PMIN(1).GT.PSYS(1,4).OR.
     &PMIN(2).GT.PSYS(2,4)) THEN
        MINT(51)=1
        RETURN
      ENDIF
 
C...Loop over two remnants; skip if none there.
      I=NS
      DO 210 JT=1,2
      ISN(JT)=0
      IF(MINT(40+JT).EQ.1.AND.MSTP(11).EQ.0) GOTO 210
      IF(JT.EQ.1) IPU=IPU1
      IF(JT.EQ.2) IPU=IPU2
 
C...Store first remnant parton.
      I=I+1
      IS(JT)=I
      ISN(JT)=1
      DO 150 J=1,5
      K(I,J)=0
      P(I,J)=0.
  150 V(I,J)=0.
      K(I,1)=1
      K(I,2)=KFLSP(JT)
      K(I,3)=MINT(83)+JT
      P(I,5)=ULMASS(K(I,2))
 
C...First parton colour connections and kinematics.
      KCOL=KCHG(LUCOMP(KFLSP(JT)),2)
      IF(KCOL.NE.0) THEN
        K(I,1)=3
        KFLS=(3-KCOL*ISIGN(1,KFLSP(JT)))/2
        K(I,KFLS+3)=IPU
        K(IPU,6-KFLS)=MOD(K(IPU,6-KFLS),MSTU(5))+MSTU(5)*I
      ENDIF
      IF(KFLCH(JT).EQ.0) THEN
        P(I,1)=-P(MINT(83)+JT+2,1)
        P(I,2)=-P(MINT(83)+JT+2,2)
        PMS(JT)=P(I,5)**2+P(I,1)**2+P(I,2)**2
        PSYS(JT,3)=SQRT(MAX(0.,PSYS(JT,4)**2-PMS(JT)))*(-1)**(JT-1)
        P(I,3)=PSYS(JT,3)
        P(I,4)=PSYS(JT,4)
 
C...When extra remnant parton or hadron: store extra remnant.
      ELSE
        I=I+1
        ISN(JT)=2
        DO 160 J=1,5
        K(I,J)=0
        P(I,J)=0.
  160   V(I,J)=0.
        K(I,1)=1
        K(I,2)=KFLCH(JT)
        K(I,3)=MINT(83)+JT
        P(I,5)=ULMASS(K(I,2))
 
C...Find parton colour connections of extra remnant.
        KCOL=KCHG(LUCOMP(KFLCH(JT)),2)
        IF(KCOL.EQ.2) THEN
          K(I,1)=3
          K(I,4)=MSTU(5)*IPU+IPU
          K(I,5)=MSTU(5)*IPU+IPU
          K(IPU,4)=MOD(K(IPU,4),MSTU(5))+MSTU(5)*I
          K(IPU,5)=MOD(K(IPU,5),MSTU(5))+MSTU(5)*I
        ELSEIF(KCOL.NE.0) THEN
          K(I,1)=3
          KFLS=(3-KCOL*ISIGN(1,KFLCH(JT)))/2
          K(I,KFLS+3)=IPU
          K(IPU,6-KFLS)=MOD(K(IPU,6-KFLS),MSTU(5))+MSTU(5)*I
        ENDIF
 
C...Relative transverse momentum when two remnants.
  170   CALL LUPTDI(1,P(I-1,1),P(I-1,2))
        IF(IABS(MINT(10+JT)).LT.20) THEN
          P(I-1,1)=0.
          P(I-1,2)=0.
        ENDIF
        PMS(JT+2)=P(I-1,5)**2+P(I-1,1)**2+P(I-1,2)**2
        P(I,1)=-P(MINT(83)+JT+2,1)-P(I-1,1)
        P(I,2)=-P(MINT(83)+JT+2,2)-P(I-1,2)
        PMS(JT+4)=P(I,5)**2+P(I,1)**2+P(I,2)**2
 
C...Relative distribution of electron energy into electron plus parton.
        IMB=1
        IF(MOD(MINT(10+JT)/1000,10).NE.0) IMB=2
        IF(IABS(MINT(10+JT)).LT.20) THEN
          XHRD=VINT(140+JT)
          T=ALOG(MIN(1E4,MAX(1.,VINT(52)))/0.16)
          NFE=1
          IF(VINT(52).GT.25.) NFE=2
          IF(VINT(52).GT.300.) NFE=3
          CALL PYSTGA(NFE,XHRD,T,XPGL1,XPQU1,XPQD1)
          CALL PYSTGA(NFE,0.999999,T,XPGL2,XPQU2,XPQD2)
          IF(KFLCH(JT).EQ.21) THEN
            WTMX=2.*MAX(XPGL1,XPGL2)
          ELSEIF(MOD(IABS(KFLCH(JT)),2).EQ.0) THEN
            WTMX=2.*MAX(XPQU1,XPQU2)
          ELSE
            WTMX=2.*MAX(XPQD1,XPQD2)
          ENDIF
  180     XE=XHRD**RLU(0)
          XG=MIN(0.999999,XHRD/XE)
          XPGA=1.+(1.-XE)**2
          CALL PYSTGA(NFE,XG,T,XPGL,XPQU,XPQD)
          IF(KFLCH(JT).EQ.21) THEN
            WT=XPGA*XPGL
          ELSEIF(MOD(IABS(KFLCH(JT)),2).EQ.0) THEN
            WT=XPGA*XPQU
          ELSE
            WT=XPGA*XPQD
          ENDIF
          IF(WT.LT.RLU(0)*WTMX) GOTO 180
          CHI(JT)=(XE-XHRD)/(1.-XHRD)
 
C...Relative distribution of energy for particle into two jets.
        ELSEIF(IABS(KFLCH(JT)).LE.10.OR.KFLCH(JT).EQ.21) THEN
          CHIK=PARP(92+2*IMB)
          IF(MSTP(92).LE.1) THEN
            IF(IMB.EQ.1) CHI(JT)=RLU(0)
            IF(IMB.EQ.2) CHI(JT)=1.-SQRT(RLU(0))
          ELSEIF(MSTP(92).EQ.2) THEN
            CHI(JT)=1.-RLU(0)**(1./(1.+CHIK))
          ELSEIF(MSTP(92).EQ.3) THEN
            CUT=2.*0.3/VINT(1)
  190       CHI(JT)=RLU(0)**2
            IF((CHI(JT)**2/(CHI(JT)**2+CUT**2))**0.25*(1.-CHI(JT))**CHIK
     &      .LT.RLU(0)) GOTO 190
          ELSE
            CUT=2.*0.3/VINT(1)
            CUTR=(1.+SQRT(1.+CUT**2))/CUT
  200       CHIR=CUT*CUTR**RLU(0)
            CHI(JT)=(CHIR**2-CUT**2)/(2.*CHIR)
            IF((1.-CHI(JT))**CHIK.LT.RLU(0)) GOTO 200
          ENDIF
 
C...Relative distribution of energy for particle into jet plus particle.
        ELSE
          IF(MSTP(92).LE.1) THEN
            IF(IMB.EQ.1) CHI(JT)=RLU(0)
            IF(IMB.EQ.2) CHI(JT)=1.-SQRT(RLU(0))
          ELSE
            CHI(JT)=1.-RLU(0)**(1./(1.+PARP(93+2*IMB)))
          ENDIF
          IF(MOD(KFLCH(JT)/1000,10).NE.0) CHI(JT)=1.-CHI(JT)
        ENDIF
 
C...Construct total transverse mass; reject if too large.
        PMS(JT)=PMS(JT+4)/CHI(JT)+PMS(JT+2)/(1.-CHI(JT))
        IF(PMS(JT).GT.PSYS(JT,4)**2) GOTO 170
        PSYS(JT,3)=SQRT(PSYS(JT,4)**2-PMS(JT))*(-1)**(JT-1)
 
C...Subdivide longitudinal momentum according to value selected above.
        PW1=CHI(JT)*(PSYS(JT,4)+ABS(PSYS(JT,3)))
        P(IS(JT)+1,4)=0.5*(PW1+PMS(JT+4)/PW1)
        P(IS(JT)+1,3)=0.5*(PW1-PMS(JT+4)/PW1)*(-1)**(JT-1)
        P(IS(JT),4)=PSYS(JT,4)-P(IS(JT)+1,4)
        P(IS(JT),3)=PSYS(JT,3)-P(IS(JT)+1,3)
      ENDIF
  210 CONTINUE
      N=I
 
C...Check if longitudinal boosts needed - if so pick two systems.
      PDEV=ABS(PSYS(0,4)+PSYS(1,4)+PSYS(2,4)-VINT(1))+
     &ABS(PSYS(0,3)+PSYS(1,3)+PSYS(2,3))
      IF(PDEV.LE.1E-6*VINT(1)) RETURN
      IF(VINT(143).GT.0.2.AND.VINT(144).GT.0.2) THEN
        IR=1
        IL=2
      ELSEIF(VINT(143).GT.0.2) THEN
        IR=1
        IL=0
      ELSEIF(VINT(144).GT.0.2) THEN
        IR=0
        IL=2
      ELSEIF(PMS(1)/PSYS(1,4)**2.GT.PMS(2)/PSYS(2,4)**2) THEN
        IR=1
        IL=0
      ELSE
        IR=0
        IL=2
      ENDIF
      IG=3-IR-IL
 
C...Construct longitudinal boosts.
      IF((IG.EQ.1.AND.ISN(1).EQ.0).OR.(IG.EQ.2.AND.ISN(2).EQ.0)) THEN
        PPB=VINT(1)
        PNB=VINT(1)
      ELSE
        PPB=VINT(1)-PSYS(IG,4)-PSYS(IG,3)
        PNB=VINT(1)-PSYS(IG,4)+PSYS(IG,3)
      ENDIF
      PMTB=PPB*PNB
      PMTR=PMS(IR)
      PMTL=PMS(IL)
      SQLAM=SQRT(MAX(0.,(PMTB-PMTR-PMTL)**2-4.*PMTR*PMTL))
      SQSGN=SIGN(1.,PSYS(IR,3)*PSYS(IL,4)-PSYS(IL,3)*PSYS(IR,4))
      RKR=(PMTB+PMTR-PMTL+SQLAM*SQSGN)/(2.*(PSYS(IR,4)+PSYS(IR,3))*PNB)
      RKL=(PMTB+PMTL-PMTR+SQLAM*SQSGN)/(2.*(PSYS(IL,4)-PSYS(IL,3))*PPB)
      BER=(RKR**2-1.)/(RKR**2+1.)
      BEL=-(RKL**2-1.)/(RKL**2+1.)
 
C...Perform longitudinal boosts.
      IF(IR.EQ.1) THEN
        CALL LUDBRB(IS(1),IS(1)+ISN(1)-1,0.,0.,0D0,0D0,DBLE(BER))
      ELSE
        CALL LUDBRB(I1,NS,0.,0.,0D0,0D0,DBLE(BER))
      ENDIF
      IF(IL.EQ.2) THEN
        CALL LUDBRB(IS(2),IS(2)+ISN(2)-1,0.,0.,0D0,0D0,DBLE(BEL))
      ELSE
        CALL LUDBRB(I1,NS,0.,0.,0D0,0D0,DBLE(BEL))
      ENDIF
 
      RETURN
      END
